/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"


/*--------------------------------------------------------------------------------------
  Checked reference counts. 
  - We use a sticky range above `RC_STICKY_LO` to prevent overflow
    of the reference count. Any sticky reference won't be freed. There is a range between
    `RC_STICKY_LO` and `RC_STICKY_HI` to ensure stickyness even with concurrent increments and decrements.
  - The range above `RC_SHARED` uses atomic operations for shared reference counts. If a decrement
    falls to `RC_SHARED` the object is freed (if is actually was shared, i.e. `thread_shared` is true).
  - Since `RC_SHARED` has the msb set, we can efficiently test in `drop` for either `0` (=free) or
    the need for atomic operations by using `if ((int32_t)rc <= 0) ...` (and similarly for `dup`).
  
  0                         : unique reference
  0x00000001 - 0x7FFFFFFF   : reference (in a single thread)
  0x80000000 - 0xBFFFFFFF   : reference or thread-shared reference (if `thread_shared`). Use atomic operations
  0xD0000000 - 0xDFFFFFFF   : sticky range: still increments, but no decrements
  0xE0000000 - 0xEFFFFFFF   : sticky range: neither increment, nor decrement
  0xF0000000 - 0xFFFFFFFF   : invalid; used for debug checks
--------------------------------------------------------------------------------------*/

#define RC_SHARED     U32(0x80000000)  // 0b1000 0000 ...
#define RC_STICKY_LO  U32(0xD0000000)  // 0b1101 0000 ...
#define RC_STICKY_HI  U32(0xE0000000)  // 0b1110 0000 ...

static inline uint32_t atomic_incr(block_t* b) {
  return atomic_increment32((volatile _Atomic(uint32_t)*)&b->header.refcount);
}
static inline uint32_t atomic_decr(block_t* b) {
  return atomic_decrement32((volatile _Atomic(uint32_t)*)&b->header.refcount);
}

// Check if a reference decrement caused the block to be free or needs atomic operations
noinline void block_check_free(block_t* b, context_t* ctx) {
  assert_internal(b!=NULL);
  assert_internal(b->header.refcount == 0 || b->header.refcount >= RC_SHARED);
  if (b->header.refcount==0) {
    block_free(b, ctx);  // no more references, free it.
  }
  else {
    const uint32_t rc = atomic_decr(b);
    if (unlikely(rc >= RC_STICKY_LO)) {
      atomic_incr(b);       // sticky: undo the decrement so we never free
    }
    else if (rc == RC_SHARED && b->header.thread_shared) {  // with a shared reference dropping to RC_SHARED means no more references
      b->header.refcount = 0; // no longer shared
      b->header.thread_shared = 0;
      block_free(b, ctx);            // no more references, free it.
    }
  }
}

noinline block_t* dup_block_check(block_t* b) {
  assert_internal(b!=NULL);
  assert_internal(b->header.refcount >= RC_SHARED);
  const uint32_t rc = atomic_incr(b);
  if (unlikely(rc >= RC_STICKY_HI)) {
    atomic_decr(b);  // undo the increment to avoid overflow
  }
  return b;
}


/*--------------------------------------------------------------------------------------
  Decrementing reference counts
  When freeing a block, we need to decrease reference counts of its children
  recursively. We carefully optimize to use no stack space in case of single field
  chains (like lists) and recurse to limited depth in other cases, using a
  `delayed_free` list in the thread local data. The `delayed_free` list is
  encoded in the headers and thus needs no allocation.
--------------------------------------------------------------------------------------*/

// Decrement a shared refcount without freeing the block yet. Returns true if there are no more references.
static bool block_check_decref_no_free(block_t* b) {
  const uint32_t rc = atomic_decr(b);
  if (rc == RC_SHARED && b->header.thread_shared) {
    b->header.refcount = 0;      // no more shared
    b->header.thread_shared = 0;   
    return true;                   // no more references
  }
  if (unlikely(rc > RC_STICKY_LO)) {
    atomic_incr(b);                // sticky: undo the decrement to never free
  }
  return false;  
}

// Decrement a refcount without freeing the block yet. Returns true if there are no more references.
static bool block_decref_no_free(block_t* b) {
  uint32_t rc = b->header.refcount;
  if (rc==0) return true;
  else if (rc >= RC_SHARED) return block_check_decref_no_free(b);
  b->header.refcount = rc - 1;
  return false;
}

// Push a block on the delayed-free list
static void block_push_delayed_free(block_t* b, context_t* ctx) {
  assert_internal(b->header.refcount == 0);
  block_t* delayed = ctx->delayed_free;
  // encode the next pointer into the block header (while keeping `scan_fsize` valid)
  b->header.refcount = (uint32_t)((uintx_t)delayed);
#if (INTPTR_SIZE > 4)
  b->header.tag = (uint16_t)(sar((intx_t)delayed,32));
  assert_internal(sar((intx_t)delayed,48) == 0 || sar((intx_t)delayed, 48) == -1);
#endif
  ctx->delayed_free = b;
}

static void block_free_raw(block_t* b);
static noinline void block_decref_free(block_t* b, size_t depth, context_t* ctx);

// Free all delayed free blocks.
// TODO: limit to a certain number to limit worst-case free times?
static void block_decref_delayed(context_t* ctx) {
  block_t* delayed;
  while ((delayed = ctx->delayed_free) != NULL) {
    ctx->delayed_free = NULL;
    do {
      block_t* b = delayed;
      // decode the next element in the delayed list from the block header
      intx_t next = (intx_t)b->header.refcount;
#if (INTPTR_SIZE>4)
      next += ((intx_t)((int16_t)(b->header.tag)) << 32); // sign extended
#endif
#ifndef NDEBUG
      b->header.refcount = 0;
#endif
      delayed = (block_t*)next;
      // and free the block
      block_decref_free(b, 0, ctx);
    } while (delayed != NULL);
  }
}

#define MAX_RECURSE_DEPTH (100)

typedef struct block_fields_s {
  block_t _block;
  box_t   fields[1];
} block_fields_t;

static inline box_t block_field(block_t* b, size_t index) {
  block_fields_t* bf = (block_fields_t*)b;  // must overlap with datatypes with scanned fields.
  return bf->fields[index];
}

// Free recursively a block -- if the recursion becomes too deep, push
// blocks on the delayed free list to free them later. The delayed free list
// is encoded in the headers and needs no further space.
static noinline void block_decref_free(block_t* b, size_t depth, context_t* ctx) {
  while(true) {
    assert_internal(b->header.refcount == 0);
    size_t scan_fsize = b->header.scan_fsize;
    if (scan_fsize == 0) {
      // nothing to scan, just free
      if (tag_is_raw(block_tag(b))) block_free_raw(b); // potentially call custom `free` function on the data
      runtime_free(b);
      return;
    }
    else if (scan_fsize == 1) {
      // if just one field, we can recursively free without using stack space
      const box_t v = block_field(b, 0);;
      runtime_free(b);
      if (is_non_null_ptr(v)) {
        // try to free the child now
        b = unbox_ptr(v);
        if (block_decref_no_free(b)) {
          // continue freeing on this block
          continue; // tailcall
        }
      }
      return;
    }
    else {
      // more than 1 field
      if (depth < MAX_RECURSE_DEPTH) {
        size_t i = 0;
        if (unlikely(scan_fsize >= SCAN_FSIZE_MAX)) { 
          scan_fsize = unbox_enum(block_field(b, 0)); 
          i++;
        }
        // free fields up to the last one
        for (; i < (scan_fsize-1); i++) {
          box_t v = block_field(b, i);
          if (is_non_null_ptr(v)) {
            block_t* vb = unbox_ptr(v);
            if (block_decref_no_free(vb)) {
              block_decref_free(vb, depth+1, ctx); // recurse with increased depth
            }
          }
        }
        // and recurse into the last one
        box_t v = block_field(b,scan_fsize - 1);
        runtime_free(b);
        if (is_non_null_ptr(v)) {
          b = unbox_ptr(v);
          if (block_decref_no_free(b)) {
            continue; // tailcall
          }
        }
        return;
      }
      else {
        // recursed too deep, push this block onto the todo list
        block_push_delayed_free(b,ctx);
        return;
      }
    }
  }
}

static void block_free_raw(block_t* b) {
  assert_internal(tag_is_raw(block_tag(b)));
  struct cptr_raw_s* raw = (struct cptr_raw_s*)b;  // all raw structures must overlap this!
  if (raw->free != NULL) {
    (*raw->free)(raw->cptr);
  }
}

// Free a block and recursively decrement reference counts on children.
void block_free(block_t* b, context_t* ctx) {
  assert_internal(b->header.refcount == 0);
  if (b->header.scan_fsize==0) {
    if (tag_is_raw(block_tag(b))) block_free_raw(b);
    runtime_free(b); // deallocate directly if nothing to scan
  }
  else {
    block_decref_free(b, 0 /* depth */, ctx);  // free recursively
    block_decref_delayed(ctx);     // process delayed frees
  }
}


