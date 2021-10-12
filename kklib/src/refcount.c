/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

// static void kk_block_drop_free_delayed(kk_context_t* ctx);
// static kk_decl_noinline void kk_block_drop_free_rec(kk_block_t* b, kk_ssize_t scan_fsize, const kk_ssize_t depth, kk_context_t* ctx);
static kk_decl_noinline void kk_block_drop_free_recx(kk_block_t* b, kk_context_t* ctx);

static void kk_block_free_raw(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_tag_is_raw(kk_block_tag(b)));
  struct kk_cptr_raw_s* raw = (struct kk_cptr_raw_s*)b;  // all raw structures must overlap this!
  if (raw->free != NULL) {
    (*raw->free)(raw->cptr, b, ctx);
  }
}

// Free a block and recursively decrement reference counts on children.
static void kk_block_drop_free(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(b->header.refcount == 0);
  const kk_ssize_t scan_fsize = b->header.scan_fsize;
  if (scan_fsize==0) {
    if (kk_tag_is_raw(kk_block_tag(b))) { kk_block_free_raw(b,ctx); }
    kk_block_free(b); // deallocate directly if nothing to scan
  }
  else {
    kk_block_drop_free_recx(b, ctx); // free recursively
    //kk_block_drop_free_rec(b, scan_fsize, 0 /* depth */, ctx);  // free recursively
    //kk_block_drop_free_delayed(ctx);     // process delayed frees
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
#define RC_INVALID    KU32(0xF0000000)  // 0b1111 ...

static inline uint32_t kk_atomic_incr(kk_block_t* b) {
  return kk_atomic_inc32_relaxed((_Atomic(uint32_t)*)&b->header.refcount);
}
static inline uint32_t kk_atomic_decr(kk_block_t* b) {
  return kk_atomic_dec32_relaxed((_Atomic(uint32_t)*)&b->header.refcount);
}
static void kk_block_make_shared(kk_block_t* b) {
  b->header.thread_shared = true;
  kk_atomic_add32_relaxed((_Atomic(uint32_t)*)&b->header.refcount, RC_SHARED+1);
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
    kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
    for (kk_ssize_t i = 0; i < scan_fsize; i++) {
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
static kk_decl_noinline bool block_check_decref_no_free(kk_block_t* b) {
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



//-----------------------------------------------------------------------------------------
// Drop a block and its children without using further stack space.
// 
// The stack-less algorithm without delay list was initially created by Anton Lorenzen.
// It maintains a parent stack in the first field of visited objects, with the 
// next field to process in as an 8-bit value in the refcount.
//-----------------------------------------------------------------------------------------

// Check if a field `i` in a block `b` should be freed, i.e. it is heap allocated with a refcount of 0.
// Optimizes by already freeing leaf blocks that are heap allocated but have no scan fields.
static inline kk_block_t* kk_block_field_should_free(kk_block_t* b, kk_ssize_t field, kk_context_t* ctx)
{
  kk_box_t v = kk_block_field(b, field);
  if (kk_box_is_non_null_ptr(v)) {
    kk_block_t* child = kk_ptr_unbox(v);
    if (kk_block_decref_no_free(child)) {
      uint8_t v_scan_fsize = child->header.scan_fsize;
      if (v_scan_fsize == 0) {
        // free leaf nodes directly and pretend it was not a ptr field
        if (kk_unlikely(kk_tag_is_raw(kk_block_tag(child)))) { kk_block_free_raw(child, ctx); } // potentially call custom `free` function on the data
        kk_block_free(child);        
      }
      else {
        return child;
      }
    }
  }
  return NULL;
}


// Drop a large block (e.g. kk_vector_t) recursively. This is only used 
// for vectors with > 255 (KK_SCAN_FSIZE_MAX) elements.
// We just iterate through the elements which is efficient but we use the stack
// for each recursion over these vectors; the idea is that vectors will not be 
// deeply nested. (We could improve this by only recursing for vectors with more than 2^32 elements 
// (fits in a refcount)).
static kk_decl_noinline void kk_block_drop_free_large_rec(kk_block_t* b, kk_ssize_t scan_fsize, kk_context_t* ctx) 
{
  kk_assert_internal(b->header.scan_fsize == KK_SCAN_FSIZE_MAX);
  for (kk_ssize_t i = 1; i < scan_fsize; i++) {   // start at 1 to skip the initial large scan_fsize field
    kk_block_t* child = kk_block_field_should_free(b, i, ctx);
    if (child != NULL) {
      // free field recursively
      kk_block_drop_free_recx(child, ctx);        
    }
  }
  // and free the vector itself
  kk_block_free(b);
}

// Recursively free a block and drop its children without using stack space
static kk_decl_noinline void kk_block_drop_free_recx(kk_block_t* b, kk_context_t* ctx) 
{
  kk_block_t* parent = NULL;
  uint8_t scan_fsize;
  
  // ------- move down ------------
  movedown:
    scan_fsize = b->header.scan_fsize;
    kk_assert_internal(b->header.refcount == 0);
    if (scan_fsize == 0) {
      // nothing to scan, just free
      if (kk_unlikely(kk_tag_is_raw(kk_block_tag(b)))) kk_block_free_raw(b, ctx); // potentially call custom `free` function on the data
      kk_block_free(b);
      // goto moveup;  // fall through
    }
    else if (scan_fsize == 1) {
      // if just one field, we can free directly and continue with the child
      const kk_box_t v = kk_block_field(b, 0);
      kk_block_free(b);
      if (kk_box_is_non_null_ptr(v)) {
        // try to free the child now
        b = kk_ptr_unbox(v);
        if (kk_block_decref_no_free(b)) {
          // continue freeing with the child block (leaving parent unchanged)
          scan_fsize = b->header.scan_fsize;
          goto movedown;
        }
      }
      // goto moveup;  // fall through
    }
    else if (scan_fsize == 2 && !kk_box_is_non_null_ptr(kk_block_field(b,0))) {
      // optimized code for lists/nodes with boxed first element
      kk_block_t* next = kk_block_field_should_free(b, 1, ctx);
      kk_block_free(b);
      if (next != NULL) {
        b = next;
        goto movedown;
      }
      // goto moveup; // fallthrough
    }
    else if (scan_fsize < KK_SCAN_FSIZE_MAX) {
      // small block more than 1 field (but less then KK_SCAN_FSIZE_MAX)
      uint8_t i = 0;
      kk_assert_internal(i < scan_fsize);
      // drop each field
      do {
        kk_block_t* child = kk_block_field_should_free(b, i, ctx);
        i++;
        if (child != NULL) {
          // go down into the child
          if (i < scan_fsize) {
            // save our progress to continue here later (when moving up)
            kk_block_field_set(b, 0, _kk_box_new_ptr(parent)); // set parent (use low-level box as parent could be NULL)
            b->header.refcount = i;
            parent = b;
          }
          else {
            // the last field: free the block and continue with the child leaving the parent unchanged
            kk_block_free(b);
          }
          // and continue with the child
          b = child;
          goto movedown;
        }
      } while (i < scan_fsize);
      // goto moveup; // fallthrough
    }
    else {
      kk_assert_internal(scan_fsize == KK_SCAN_FSIZE_MAX);
      // is it a small vector?
      kk_ssize_t vscan_fsize = (kk_ssize_t)kk_int_unbox(kk_block_field(b, 0));
      if (vscan_fsize < KK_SCAN_FSIZE_MAX) {
        // todo: this will never happen as we initialize this already in `block_large_alloc` ?
        // pretend it is a small block (which is ok as the scan field itself is boxed)
        // this way do not consume stack for small vectors
        b->header.scan_fsize = (uint8_t)(vscan_fsize);
        goto movedown;
      }
      else {
        // recurse over large blocks
        kk_block_drop_free_large_rec(b, vscan_fsize, ctx);
      }
    }

  // ------- move up along the parent chain ------------
  while (kk_likely(parent != NULL)) {
    // go up to parent
    uint8_t i = (uint8_t)parent->header.refcount;
    scan_fsize = parent->header.scan_fsize;
    kk_assert_internal(i < scan_fsize);
    // go through children of the parent
    do {
      kk_block_t* child = kk_block_field_should_free(parent, i, ctx);
      i++;
      if (child != NULL) {
        if (i < scan_fsize) {
          // save our progress to continue here later
          parent->header.refcount = i;
        }
        else {
          // the last field; free the block and move the parent one up
          b = parent;
          parent = _kk_box_ptr(kk_block_field(parent, 0)); // use low-level box as it can be NULL
          kk_block_free(b);  // note: cannot be a raw block as those have no scanfield
        }
        // and continue with the child
        b = child;
        goto movedown;        
      }
    } while (i < scan_fsize);
    // done: free the block and move up further
    b = parent;
    parent = kk_ptr_unbox( kk_block_field(parent, 0) );
    kk_block_free(b);  // note: cannot be a raw block as those have no scanfield
  }
  // done
}


//-----------------------------------------------------------------------------------------
// Mark a block and all children recursively as thread shared
//-----------------------------------------------------------------------------------------

#define MAX_RECURSE_DEPTH (100)

// Stackless marking is more expensive so we switch to this only after recursing first.
static kk_decl_noinline void kk_block_mark_shared_recx(kk_block_t* b, kk_context_t* ctx);


// Check if a field `i` in a block `b` should be marked, i.e. it is heap allocated and not yet thread shared.
// Optimizes by already marking leaf blocks that have no scan fields.
static inline kk_block_t* kk_block_field_should_mark(kk_block_t* b, kk_ssize_t field, kk_context_t* ctx)
{
  KK_UNUSED(ctx);
  kk_box_t v = kk_block_field(b, field);
  if (kk_box_is_non_null_ptr(v)) {
    kk_block_t* child = kk_ptr_unbox(v);
    if (!child->header.thread_shared) {
      if (child->header.scan_fsize == 0) {
        // mark leaf objects directly as shared
        kk_block_make_shared(child);
      }
      else {
        return child;
      }
    }
  }
  return NULL;
}
  
// Recurse up to `depth` while marking objects
static kk_decl_noinline void kk_block_mark_shared_rec(kk_block_t* b, kk_ssize_t scan_fsize, const kk_ssize_t depth, kk_context_t* ctx) {
  while(true) {
    if (b->header.thread_shared) {
      // already shared
      return;
    } 
    if (scan_fsize == 0) {
      // nothing to scan
      kk_block_make_shared(b);
      return;
    }
    else if (scan_fsize == 1) {
      // if just one field, we can recursively scan without using stack space
      kk_block_make_shared(b);
      const kk_box_t v = kk_block_field(b, 0);
      if (kk_box_is_non_null_ptr(v)) {
        // try to mark the child now
        b = kk_ptr_unbox(v);
        scan_fsize = b->header.scan_fsize;
        continue; // tailcall
      }
      return;
    }
    else {
      // more than 1 field
      if (depth < MAX_RECURSE_DEPTH) {
        kk_block_make_shared(b);
        kk_ssize_t i = 0;
        if (kk_unlikely(scan_fsize >= KK_SCAN_FSIZE_MAX)) { 
          scan_fsize = (kk_ssize_t)kk_int_unbox(kk_block_field(b, 0)); 
          i++;  // skip scan field
        }
        // mark fields up to the last one
        for (; i < (scan_fsize-1); i++) {
          kk_box_t v = kk_block_field(b, i);
          if (kk_box_is_non_null_ptr(v)) {
            kk_block_t* vb = kk_ptr_unbox(v);
            kk_block_mark_shared_rec(vb, vb->header.scan_fsize, depth+1, ctx); // recurse with increased depth
          }
        }
        // and recurse into the last one
        kk_box_t v = kk_block_field(b,scan_fsize - 1);
        if (kk_box_is_non_null_ptr(v)) {
          b = kk_ptr_unbox(v);
          scan_fsize = b->header.scan_fsize;
          continue; // tailcall          
        }
        return;
      }
      else {
        // switch to stackless marking
        kk_block_mark_shared_recx(b, ctx);
        return;
      }
    }
  }
}



// mark a large vector
static kk_decl_noinline void kk_block_mark_shared_recx_large(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(b->header.scan_fsize == KK_SCAN_FSIZE_MAX);
  kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
  for (kk_ssize_t i = 1; i < scan_fsize; i++) {  // start at 1 to skip the large scan field itself
    if (kk_block_field_should_mark(b, i, ctx)) {
      kk_block_mark_shared_recx(b, ctx);
    }
  }
  kk_block_make_shared(b);
}

// Stackless marking by using pointer reversal
static kk_decl_noinline void kk_block_mark_shared_recx(kk_block_t* b, kk_context_t* ctx) 
{
  kk_block_t* parent = NULL;
  if (b->header.thread_shared) return;
  if (b->header.scan_fsize == 0) return;
  uint8_t i = 0;
  uint8_t scan_fsize = b->header.scan_fsize;

  // ---- marking fields -----
markfields:
  kk_assert(!b->header.thread_shared);  
  kk_assert_internal(scan_fsize > 0);
  if (scan_fsize == KK_SCAN_FSIZE_MAX) {
    // recurse over the stack for large objects (vectors)
    kk_block_mark_shared_recx_large(b, ctx);
  }
  else {
    do {
      kk_block_t* child = kk_block_field_should_mark(b, i, ctx);
      i++;
      if (child != NULL) {
        // move down 
        // remember our state and link back to the parent
        kk_block_field_set(b, i-1, _kk_box_new_ptr(parent));
        parent = b;
        parent->header.thread_shared = i;
        b = child;
        i = 0;
        scan_fsize = b->header.scan_fsize;
        goto markfields;
      }
    } while (i < scan_fsize);
    kk_block_make_shared(b);
  }

  //--- moving back up ------------------
  while (parent != NULL) {
    // move up
    i = parent->header.thread_shared;
    kk_block_t* pparent = _kk_box_ptr( kk_block_field(parent, i-1) );
    kk_block_field_set(parent, i-1, kk_ptr_box(b));  // restore original pointer
    parent = pparent;
    b = parent;
    // and continue visiting the fields
    scan_fsize = b->header.scan_fsize;
    if (i >= scan_fsize) {
      kk_assert_internal(i == scan_fsize);
      // done, keep moving up
      kk_block_make_shared(b);
    }
    else {
      // mark the rest of the fields starting at `i` upto `scan_fsize`
      goto markfields;
    }
  }
  // done
}


kk_decl_export void kk_block_mark_shared( kk_block_t* b, kk_context_t* ctx ) {
  if (!b->header.thread_shared) {
    kk_block_mark_shared_rec(b, b->header.scan_fsize, 0, ctx);
  }
}

kk_decl_export void kk_box_mark_shared( kk_box_t b, kk_context_t* ctx ) {
  if (kk_box_is_non_null_ptr(b)) {
    kk_block_mark_shared( kk_ptr_unbox(b), ctx );
  }
}

#if 0
//-----------------------------------------------------------------------------------------
// Old: Drop a block and its children using at most MAX_RECURSE_DEPTH stack space
//-----------------------------------------------------------------------------------------

// Push a block on the delayed-free list
static void kk_block_push_delayed_drop_free(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(b->header.refcount == 0);
  kk_block_t* delayed = ctx->delayed_free;
  // encode the next pointer into the block header (while keeping `scan_fsize` valid)
  b->header.refcount = (uint32_t)((uintptr_t)delayed);
#if (KK_INTPTR_SIZE > 4)
  b->header.tag = (uint16_t)((uintptr_t)delayed >> 32);  // at most 48 bits, but can extend to 56 bits (as only scan_fsize needs to be preserved)
  kk_assert_internal(((uintptr_t)delayed >> 48) == 0);   // adapt for sign extension?
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
      uintptr_t next = (uintptr_t)b->header.refcount;
#if (KK_INTPTR_SIZE>4)
      next += (uintptr_t)(b->header.tag) << 32;
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

// Free recursively a block -- if the recursion becomes too deep, push
// blocks on the delayed free list to free them later. The delayed free list
// is encoded in the headers and needs no further space.
static kk_decl_noinline void kk_block_drop_free_rec(kk_block_t* b, kk_ssize_t scan_fsize, const kk_ssize_t depth, kk_context_t* ctx) {
  while (true) {
    kk_assert_internal(b->header.refcount == 0);
    if (scan_fsize == 0) {
      // nothing to scan, just free
      if (kk_tag_is_raw(kk_block_tag(b))) kk_block_free_raw(b, ctx); // potentially call custom `free` function on the data
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
        kk_ssize_t i = 0;
        if (kk_unlikely(scan_fsize >= KK_SCAN_FSIZE_MAX)) {
          scan_fsize = (kk_ssize_t)kk_int_unbox(kk_block_field(b, 0)); 
          i++;  // skip scan field
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
        kk_box_t v = kk_block_field(b, scan_fsize - 1);
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
        kk_block_push_delayed_drop_free(b, ctx);
        return;
      }
    }
  }
}

#endif