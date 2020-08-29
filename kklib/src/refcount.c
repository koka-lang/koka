/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

static void kk_block_drop_free_delayed(kk_context_t* ctx);
static kk_decl_noinline void kk_block_drop_free_rec(kk_block_t* b, size_t scan_fsize, const size_t depth, kk_context_t* ctx);

static void kk_block_free_raw(kk_block_t* b) {
  kk_assert_internal(kk_tag_is_raw(kk_block_tag(b)));
  struct kk_cptr_raw_s* raw = (struct kk_cptr_raw_s*)b;  // all raw structures must overlap this!
  if (raw->free != NULL) {
    (*raw->free)(raw->cptr);
  }
}

// Free a block and recursively decrement reference counts on children.
static void kk_block_drop_free(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(b->header.refcount == 0);
  const size_t scan_fsize = b->header.scan_fsize;
  if (scan_fsize==0) {
    if (kk_tag_is_raw(kk_block_tag(b))) kk_block_free_raw(b);
    kk_block_free(b); // deallocate directly if nothing to scan
  }
  else {
    kk_block_drop_free_rec(b, scan_fsize, 0 /* depth */, ctx);  // free recursively
    kk_block_drop_free_delayed(ctx);     // process delayed frees
  }
}



/*--------------------------------------------------------------------------------------
  Checked reference counts. 
  - We use a sticky range above `RC_STICKY_LO` to prevent overflow
    of the reference count. Any sticky reference won't be freed. There is a range between
    `RC_STICKY_LO` and `RC_STICKY_HI` to ensure stickyness even with concurrent increments and decrements.
  - The range above `RC_SHARED` uses atomic operations for shared reference counts. If a decrement
    falls to `RC_SHARED` the object is freed (if is actually was shared, i.e. `kk_thread_shared` is true).
  - Since `RC_SHARED` has the msb set, we can efficiently test in `drop` for either `0` (=free) or
    the need for atomic operations by using `if ((int32_t)rc <= 0) ...` (and similarly for `dup`).
  
  0                         : unique reference
  0x00000001 - 0x7FFFFFFF   : reference (in a single thread)
  0x80000000 - 0xCFFFFFFF   : reference or thread-shared reference (if `kk_thread_shared`). Use atomic operations
  0xD0000000 - 0xDFFFFFFF   : sticky range: still increments, but no decrements
  0xE0000000 - 0xEFFFFFFF   : sticky range: neither increment, nor decrement
  0xF0000000 - 0xFFFFFFFF   : invalid; used for debug checks
--------------------------------------------------------------------------------------*/

#define RC_SHARED     KU32(0x80000000)  // 0b1000 ...
#define RC_STICKY_LO  KU32(0xD0000000)  // 0b1101 ...
#define RC_STICKY_HI  KU32(0xE0000000)  // 0b1110 ...
// #define RC_INVALID    KU32(0xF0000000)  // 0b1111 ...

static inline uint32_t kk_atomic_incr(kk_block_t* b) {
  return kk_atomic_inc32_relaxed((_Atomic(uint32_t)*)&b->header.refcount);
}
static inline uint32_t kk_atomic_decr(kk_block_t* b) {
  return kk_atomic_dec32_relaxed((_Atomic(uint32_t)*)&b->header.refcount);
}

// Check if a reference decrement caused the block to be free or needs atomic operations
kk_decl_noinline void kk_block_check_drop(kk_block_t* b, uint32_t rc0, kk_context_t* ctx) {
  kk_assert_internal(b!=NULL);
  kk_assert_internal(b->header.refcount == rc0);
  kk_assert_internal(rc0 == 0 || (rc0 >= RC_SHARED && rc0 < RC_INVALID));
  if (kk_likely(rc0==0)) {
    kk_block_drop_free(b, ctx);  // no more references, free it.
  }
  else if (kk_unlikely(rc0 >= RC_STICKY_LO)) {
    // sticky: do not decrement further
  }
  else {
    const uint32_t rc = kk_atomic_decr(b);
    if (rc == RC_SHARED && b->header.thread_shared) {  // with a shared reference dropping to RC_SHARED means no more references
      b->header.refcount = 0;        // no longer shared
      b->header.thread_shared = 0;
      kk_block_drop_free(b, ctx);            // no more references, free it.
    }
  }
}

// Check if a reference decrement caused the block to be reused or needs atomic operations
kk_decl_noinline kk_reuse_t kk_block_check_drop_reuse(kk_block_t* b, uint32_t rc0, kk_context_t* ctx) {
  kk_assert_internal(b!=NULL);
  kk_assert_internal(b->header.refcount == rc0);
  kk_assert_internal(rc0 == 0 || (rc0 >= RC_SHARED && rc0 < RC_INVALID));
  if (kk_likely(rc0==0)) {
    // no more references, reuse it.
    size_t scan_fsize = kk_block_scan_fsize(b);
    for (size_t i = 0; i < scan_fsize; i++) {
      kk_box_drop(kk_block_field(b, i), ctx);
    }
    memset(&b->header, 0, sizeof(kk_header_t)); // not really necessary
    return b;
  }
  else {
    // may be shared or sticky
    kk_block_check_drop(b, rc0, ctx);
    return kk_reuse_null;
  }
}

// Check if a reference decrement caused the block to be freed shallowly or needs atomic operations
kk_decl_noinline void kk_block_check_decref(kk_block_t* b, uint32_t rc0, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  kk_assert_internal(b!=NULL);
  kk_assert_internal(b->header.refcount == rc0);
  kk_assert_internal(rc0 == 0 || (rc0 >= RC_SHARED && rc0 < RC_INVALID));
  if (kk_likely(rc0==0)) {
    kk_free(b);  // no more references, free it (without dropping children!)
  }
  else if (kk_unlikely(rc0 >= RC_STICKY_LO)) {
    // sticky: do not decrement further
  }
  else {
    const uint32_t rc = kk_atomic_decr(b);
    if (rc == RC_SHARED && b->header.thread_shared) {  // with a shared reference dropping to RC_SHARED means no more references
      b->header.refcount = 0;        // no longer shared
      b->header.thread_shared = 0;
      kk_free(b);               // no more references, free it.
    }
  }
}


kk_decl_noinline kk_block_t* kk_block_check_dup(kk_block_t* b, uint32_t rc0) {
  kk_assert_internal(b!=NULL);
  kk_assert_internal(b->header.refcount == rc0 && rc0 >= RC_SHARED);
  if (kk_likely(rc0 < RC_STICKY_HI)) {
    kk_atomic_incr(b);
  }
  // else sticky: no longer increment (or decrement)
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
static bool block_check_decref_no_free(kk_block_t* b) {
  const uint32_t rc = kk_atomic_decr(b);
  if (rc == RC_SHARED && b->header.thread_shared) {
    b->header.refcount = 0;      // no more shared
    b->header.thread_shared = 0;   
    return true;                   // no more references
  }
  if (kk_unlikely(rc > RC_STICKY_LO)) {
    kk_atomic_incr(b);                // sticky: undo the decrement to never free
  }
  return false;  
}

// Decrement a refcount without freeing the block yet. Returns true if there are no more references.
static bool kk_block_decref_no_free(kk_block_t* b) {
  uint32_t rc = b->header.refcount;
  if (rc==0) return true;
  else if (rc >= RC_SHARED) return block_check_decref_no_free(b);
  b->header.refcount = rc - 1;
  return false;
}

// Push a block on the delayed-free list
static void kk_block_push_delayed_drop_free(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(b->header.refcount == 0);
  kk_block_t* delayed = ctx->delayed_free;
  // encode the next pointer into the block header (while keeping `scan_fsize` valid)
  b->header.refcount = (uint32_t)((kk_uintx_t)delayed);
#if (KK_INTPTR_SIZE > 4)
  b->header.tag = (uint16_t)(kk_sar((kk_intx_t)delayed,32));
  kk_assert_internal(kk_sar((kk_intx_t)delayed,48) == 0 || kk_sar((kk_intx_t)delayed, 48) == -1);
#endif
  ctx->delayed_free = b;
}


// Free all delayed free blocks.
// TODO: limit to a certain number to limit worst-case free times?
static void kk_block_drop_free_delayed(kk_context_t* ctx) {
  kk_block_t* delayed;
  while ((delayed = ctx->delayed_free) != NULL) {
    ctx->delayed_free = NULL;
    do {
      kk_block_t* b = delayed;
      // decode the next element in the delayed list from the block header
      kk_intx_t next = (kk_intx_t)b->header.refcount;
#if (KK_INTPTR_SIZE>4)
      next += ((kk_intx_t)((int16_t)(b->header.tag)) << 32); // sign extended
#endif
#ifndef NDEBUG
      b->header.refcount = 0;
#endif
      delayed = (kk_block_t*)next;
      // and free the block
      kk_block_drop_free_rec(b, b->header.scan_fsize, 0, ctx);
    } while (delayed != NULL);
  }
}

#define MAX_RECURSE_DEPTH (100)

// Free recursively a block -- if the recursion becomes too deep, push
// blocks on the delayed free list to free them later. The delayed free list
// is encoded in the headers and needs no further space.
static kk_decl_noinline void kk_block_drop_free_rec(kk_block_t* b, size_t scan_fsize, const size_t depth, kk_context_t* ctx) {
  while(true) {
    kk_assert_internal(b->header.refcount == 0);
    if (scan_fsize == 0) {
      // nothing to scan, just free
      if (kk_tag_is_raw(kk_block_tag(b))) kk_block_free_raw(b); // potentially call custom `free` function on the data
      kk_block_free(b);
      return;
    }
    else if (scan_fsize == 1) {
      // if just one field, we can recursively free without using stack space
      const kk_box_t v = kk_block_field(b, 0);
      kk_block_free(b);
      if (kk_box_is_non_null_ptr(v)) {
        // try to free the child now
        b = kk_ptr_unbox(v);
        if (kk_block_decref_no_free(b)) {
          // continue freeing on this block
          scan_fsize = b->header.scan_fsize;
          continue; // tailcall
        }
      }
      return;
    }
    else {
      // more than 1 field
      if (depth < MAX_RECURSE_DEPTH) {
        size_t i = 0;
        if (kk_unlikely(scan_fsize >= KK_SCAN_FSIZE_MAX)) { 
          scan_fsize = kk_enum_unbox(kk_block_field(b, 0)); 
          i++;
        }
        // free fields up to the last one
        for (; i < (scan_fsize-1); i++) {
          kk_box_t v = kk_block_field(b, i);
          if (kk_box_is_non_null_ptr(v)) {
            kk_block_t* vb = kk_ptr_unbox(v);
            if (kk_block_decref_no_free(vb)) {
              kk_block_drop_free_rec(vb, vb->header.scan_fsize, depth+1, ctx); // recurse with increased depth
            }
          }
        }
        // and recurse into the last one
        kk_box_t v = kk_block_field(b,scan_fsize - 1);
        kk_block_free(b);
        if (kk_box_is_non_null_ptr(v)) {
          b = kk_ptr_unbox(v);
          if (kk_block_decref_no_free(b)) {
            scan_fsize = b->header.scan_fsize;
            continue; // tailcall
          }
        }
        return;
      }
      else {
        // recursed too deep, push this block onto the todo list
        kk_block_push_delayed_drop_free(b,ctx);
        return;
      }
    }
  }
}

