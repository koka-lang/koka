/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen, Anton Lorenzen

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
  kk_assert_internal(kk_block_refcount(b) == 0);
  const kk_ssize_t scan_fsize = b->header.scan_fsize;
  if (scan_fsize==0) {
    // TODO: can we avoid raw object tests?
    if kk_unlikely(kk_tag_is_raw(kk_block_tag(b))) { kk_block_free_raw(b,ctx); }
    kk_block_free(b,ctx); // deallocate directly if nothing to scan
  }
  else {
    kk_block_drop_free_recx(b, ctx); // free recursively
    // TODO: for performance unroll one iteration for scan_fsize == 1 
    // and scan_fsize == 2 with the first field an non-ptr ?    
  }
}



/*--------------------------------------------------------------------------------------
  Checked reference counts. 

  positive:
    0               : unique reference
    1 to INT32_MAX  : reference count (in a single thread)   (~2.1e9 counts)

  negative:
    INT32_MIN                                     : sticky: single-threaded stricky reference count (RC_STUCK)
    INT32_MIN+1          to INT32_MIN+0x10000000  : sticky: neither increment, nor decrement
    INT32_MIN+0x10000001 to INT32_MIN+0x20000000  : sticky: still decrements (dup) but no more increments (drop)
    INT32_MIN+0x20000001 to -2                    : thread-shared reference counts with atomic increment/decrement. (~1.6e9 counts)
    -1                                            : thread-shared reference count that is unique now (RC_SHARED_UNIQUE)

  
  0 <= refcount <= MAX_INT32 
    regular reference counts where use 0 for a unique reference. So a reference count of 10 means there
    are 11 reference (from the current thread only).
    If it is dup'd beyond MAX_INT32 it'll overflow automatically into the sticky range (as a negative value)

  MAX_INT32 < refcount <= MAX_UINT32
    Thread-shared and sticky reference counts. These use atomic increment/decrement operations.

  MIN_INT32 == RC_STUCK
    This is used for single threaded refcounts that overflow. (This is sticky and the object will never be freed)
    The thread-shared refcounts will never get there.

  RC_STICKY_DROP < refcount <= -1 (= RC_UNIQUE_SHARED)
    A thread-shared reference count.
    The reference count grows down, e.g. if there are N references to a
    thread-shared object, then the reference count is -N.
    It means that to dup a thread-shared reference will  _decrement_ the  count,
    and to drop will _increment_ the count.

  MIN_INT32 < refcount <= RC_STICKY_DROP
    The sticky range. An object in this range will never be freed anymore.
    Since we first read the reference count non-atomically we need a range
    for stickiness. Once `refcount <= RC_STICKY_DROP` it will never drop anymore
    (increment the refcount), and once refcount <= RC_STICKY it will never dup/drop anymore.
    We assume that the relaxed reads of the reference counts catch up to the atomic
    value within the sticky range (which has a range of ~0.5e9 counts).

  Atomic memory ordering:
  - Increments can be relaxed as there is no dependency on order, the owner
    could access fields just as well before or after incrementing.
  - Decrements must use release order though: after decrementing the owner should
    no longer read/write to the object so no reads/writes are allowed to be reordered
    to occur after the decrement.
  - If the decrement causes the object to be freed, we also need to acquire: any reads/writes
    that occur after the final decrement should similarly not be reordered just before it.
  - see also: https://devblogs.microsoft.com/oldnewthing/20210409-00/?p=105065
--------------------------------------------------------------------------------------*/

#define RC_STUCK          INT32_MIN
#define RC_STICKY         (RC_STUCK + 0x10000000)
#define RC_STICKY_DROP    (RC_STUCK + 0x20000000)
#define RC_SHARED_UNIQUE  KK_I32(-1)
#define RC_UNIQUE         KK_I32(0)


static inline kk_refcount_t kk_atomic_dup(kk_block_t* b) {
  return kk_atomic_dec_relaxed(&b->header.refcount);
}
static inline kk_refcount_t kk_atomic_drop(kk_block_t* b) {
  return kk_atomic_inc_release(&b->header.refcount);
}
static inline kk_refcount_t kk_atomic_acquire(kk_block_t* b) {
  return kk_atomic_load_acquire(&b->header.refcount);
}

static void kk_block_make_shared(kk_block_t* b) {
  kk_refcount_t rc = kk_block_refcount(b);
  kk_assert_internal(!kk_refcount_is_thread_shared(rc));  // not thread shared already
  if (!kk_refcount_is_thread_shared(rc)) {
    rc = -rc;                                     // cannot overflow as rc is positive
    if (rc <= RC_STICKY_DROP) { rc = RC_STICKY; } // for high reference counts default to sticky
    kk_block_refcount_set(b, rc);
  }
}

// Check if a reference dup needs an atomic operation
kk_decl_noinline kk_block_t* kk_block_check_dup(kk_block_t* b, kk_refcount_t rc0) {
  kk_assert_internal(b!=NULL);
  kk_assert_internal(kk_refcount_is_thread_shared(rc0)); // includes KK_STUCK
  if kk_likely(rc0 > RC_STICKY) {
    kk_atomic_dup(b);  // decrement
  }
  // else sticky: no longer dup (= decrement)
  return b;
}

// Check if a reference drop caused the block to be free, or needs atomic operations
// Currently compiles without register spills (on x64) which is important for performance.
// Be careful when adding more code to not induce stack usage.
kk_decl_noinline void kk_block_check_drop(kk_block_t* b, kk_refcount_t rc0, kk_context_t* ctx) {
  kk_assert_internal(b!=NULL);
  kk_assert_internal(kk_block_refcount(b) == rc0);
  kk_assert_internal(rc0 == 0 || kk_refcount_is_thread_shared(rc0));
  if kk_likely(rc0==0) {
    kk_block_drop_free(b, ctx);  // no more references, free it.
  }
  else if kk_unlikely(rc0 <= RC_STICKY_DROP) {
    // sticky: do not drop further
  }
  else {
    const kk_refcount_t rc = kk_atomic_drop(b);  // increment
    if (rc == RC_SHARED_UNIQUE) {    // this was the last reference?
      kk_atomic_acquire(b);          // prevent reordering of reads/writes before this point
      kk_block_refcount_set(b,0);    // no longer shared
      kk_block_drop_free(b, ctx);    // no more references, free it.
    }
    kk_assert_internal(rc > RC_STICKY);
  }
}

// Check if a reference decrement caused the block to be reused or needs atomic operations
kk_decl_noinline kk_reuse_t kk_block_check_drop_reuse(kk_block_t* b, kk_refcount_t rc0, kk_context_t* ctx) {
  kk_assert_internal(b!=NULL);
  kk_assert_internal(kk_block_refcount(b) == rc0);
  kk_assert_internal(rc0 == 0 || kk_refcount_is_thread_shared(rc0));
  if kk_likely(rc0==0) {
    // no more references, reuse it.
    kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
    for (kk_ssize_t i = 0; i < scan_fsize; i++) {
      kk_box_drop(kk_block_field(b, i), ctx);
    }
    kk_header_init(&b->header,0,KK_TAG_INVALID); // not really necessary
    return b;
  }
  else {
    // may be shared or sticky
    kk_block_check_drop(b, rc0, ctx);
    return kk_reuse_null;
  }
}

// Check if a reference decrement caused the block to be freed shallowly or needs atomic operations
kk_decl_noinline void kk_block_check_decref(kk_block_t* b, kk_refcount_t rc0, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_assert_internal(b!=NULL);
  kk_assert_internal(kk_block_refcount(b) == rc0);
  kk_assert_internal(rc0 == 0 || kk_refcount_is_thread_shared(rc0));
  if kk_likely(rc0==0) {
    kk_free(b,ctx);  // no more references, free it (without dropping children!)
  }
  else if kk_unlikely(rc0 <= RC_STICKY_DROP) {
    // sticky: do not decrement further
  }
  else {
    const kk_refcount_t rc = kk_atomic_drop(b);  // decrement
    if (rc == RC_SHARED_UNIQUE) {    // last referenc?
      kk_block_refcount_set(b,0);    // no longer shared
      kk_free(b,ctx);                // no more references, free it.
    }
  }
}




/*--------------------------------------------------------------------------------------
  Decrementing reference counts without free-ing  
--------------------------------------------------------------------------------------*/

// Decrement a shared refcount without freeing the block yet. Returns true if there are no more references.
static kk_decl_noinline bool block_thread_shared_decref_no_free(kk_block_t* b) {
  const kk_refcount_t rc = kk_atomic_drop(b);
  kk_assert_internal(kk_refcount_is_thread_shared(rc));
  if (rc == RC_SHARED_UNIQUE) {
    kk_block_refcount_set(b, 0); // no more shared
    return true;                 // no more references
  }
  else {
    return false;
  }
}

// Decrement a refcount without freeing the block yet. Returns true if there are no more references.
static bool kk_block_decref_no_free(kk_block_t* b) {
  kk_refcount_t rc = kk_block_refcount(b);
  if (rc==0) {
    return true;
  }
  else if kk_unlikely(kk_refcount_is_thread_shared(rc)) {
    return (rc <= RC_STICKY_DROP ? false : block_thread_shared_decref_no_free(b));
  }
  else {
    kk_block_refcount_set(b, rc - 1);
    return false;
  }
}

//-----------------------------------------------------------------------------------------
// Drop a block and its children without using further stack space.
// 
// The stack-less algorithm without delay list was initially created by Anton Lorenzen.
// It maintains a parent stack in the first field of visited objects, with the 
// next field to process as an 8-bit value in the `_field_idx` (also used by marking)
// (and it is faster than a recursive version so we only have a stackless free)
//-----------------------------------------------------------------------------------------

// Check if a field `i` in a block `b` should be freed, i.e. it is heap allocated with a refcount of 0. 
// Optimizes by already freeing leaf blocks that are heap allocated but have no scan fields. 

static inline kk_block_t* kk_block_field_should_free(kk_block_t* b, kk_ssize_t field, kk_context_t* ctx) { 
  kk_box_t v = kk_block_field(b, field); 
  if (kk_box_is_non_null_ptr(v)) {
    kk_block_t* child = kk_ptr_unbox(v,ctx); 
    kk_assert_internal(kk_block_is_valid(child));
    if (kk_block_decref_no_free(child)) {
      uint8_t v_scan_fsize = child->header.scan_fsize; 
      if (v_scan_fsize == 0) { // free leaf nodes directly and pretend it was not a ptr field 
        if kk_unlikely(kk_tag_is_raw(kk_block_tag(child))) { kk_block_free_raw(child,ctx); }  // potentially call custom `free` function on the data
        kk_block_free(child,ctx); 
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
// (which fits in a refcount)).
static kk_decl_noinline void kk_block_drop_free_large_rec(kk_block_t* b, kk_context_t* ctx) 
{
  kk_assert_internal(b->header.scan_fsize == KK_SCAN_FSIZE_MAX);
  kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
  for (kk_ssize_t i = 1; i < scan_fsize; i++) {   // start at 1 to skip the initial large scan_fsize field
    kk_block_t* child = kk_block_field_should_free(b, i, ctx);
    if (child != NULL) {
      // free field recursively
      kk_block_drop_free_recx(child, ctx);        
    }
  }
  // and free the vector itself
  kk_block_free(b,ctx);
}

// Recursively free a block and drop its children without using stack space
static kk_decl_noinline void kk_block_drop_free_recx(kk_block_t* b, kk_context_t* ctx) 
{
  kk_assert_internal(b->header.scan_fsize > 0);
  kk_block_t* parent = NULL;
  uint8_t scan_fsize;
  uint8_t i; // current field

  // ------- drop the children and free the block b ------------
  move_down:
    kk_assert_internal(kk_block_is_valid(b));
    scan_fsize = b->header.scan_fsize;
    kk_assert_internal(kk_block_refcount(b) == 0);
    kk_assert_internal(scan_fsize > 0);           // due to kk_block_should_free
    if (scan_fsize == 1) {
      // if just one field, we can free directly and continue with the child
      kk_block_t* next = kk_block_field_should_free(b, 0, ctx);
      kk_block_free(b,ctx);
      if (next != NULL) {
        b = next;
        goto move_down;
      }
      // goto move_up; // fallthrough
    }
    else if (scan_fsize == 2 && !kk_box_is_non_null_ptr(kk_block_field(b,0))) {
      // optimized code for lists/nodes with boxed first element
      kk_block_t* next = kk_block_field_should_free(b, 1, ctx);
      kk_block_free(b,ctx);
      if (next != NULL) {
        b = next;
        goto move_down;
      }
      // goto move_up; // fallthrough
    }
    else if kk_unlikely(scan_fsize == KK_SCAN_FSIZE_MAX) {
      kk_assert_internal(scan_fsize == KK_SCAN_FSIZE_MAX);
      kk_block_drop_free_large_rec(b, ctx);
      // goto move_up; // fallthrough
    }
    else {
      // small block more than 1 field (but less then KK_SCAN_FSIZE_MAX)
      i = 0;

  scan_fields:  // i points to the starting field to scan
      kk_assert_internal(i < scan_fsize);
      // drop each field
      do {
        kk_block_t* child = kk_block_field_should_free(b, i, ctx);
        i++;
        if (child != NULL) {
          // go down into the child
          if (i < scan_fsize) {
            // save our progress to continue here later (when moving up along the parent chain)
            kk_block_field_set(b, 0, kk_box_from_potential_null_ptr(parent,ctx)); // set parent (use low-level box as parent could be NULL)
            kk_block_field_idx_set(b,i);
            parent = b;
          }
          else {
            // the last field: free the block and continue with the child leaving the parent unchanged
            kk_block_free(b,ctx);
          }
          // and continue with the child
          b = child;
          goto move_down;
        }
      } while (i < scan_fsize);
      kk_block_free(b,ctx);
      // goto move_up; // fallthrough
    }
    
  // ------- move up along the parent chain ------------
  // move_up:
    if (parent != NULL) {
      b = parent;
      parent = kk_box_to_potential_null_ptr( kk_block_field(parent, 0), ctx );  // low-level unbox as it can be NULL
      scan_fsize = b->header.scan_fsize;
      i = kk_block_field_idx(b);
      kk_assert_internal(i < scan_fsize);
      goto scan_fields;
    }

  // done
}

#if 0
// Recursively free a block and drop its children without using stack space
static kk_decl_noinline void kk_block_drop_free_rec_old(kk_block_t* b, kk_context_t* ctx) 
{
  kk_assert_internal(b->header.scan_fsize > 0);
  kk_block_t* parent = NULL;
  uint8_t scan_fsize;

  // ------- move down ------------
  movedown:
    scan_fsize = b->header.scan_fsize;
    kk_assert_internal(kk_block_refcount(b) == 0);
    kk_assert_internal(scan_fsize > 0);           // due to kk_block_should_free
    if (scan_fsize == 1) {
      // if just one field, we can free directly and continue with the child
      kk_block_t* next = kk_block_field_should_free(b, 0, ctx);
      kk_block_free(b,ctx);
      if (next != NULL) {
        b = next;
        goto movedown;
      }
      // goto moveup; // fallthrough
    }
    else if (scan_fsize == 2 && !kk_box_is_non_null_ptr(kk_block_field(b,0))) {
      // optimized code for lists/nodes with boxed first element
      kk_block_t* next = kk_block_field_should_free(b, 1, ctx);
      kk_block_free(b,ctx);
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
            kk_block_field_idx_set(b,i);
            parent = b;
          }
          else {
            // the last field: free the block and continue with the child leaving the parent unchanged
            kk_block_free(b,ctx);
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
      kk_block_drop_free_large_rec(b, ctx);
    }

  // ------- move up along the parent chain ------------
  while kk_likely(parent != NULL) {
    // go up to parent
    uint8_t i = kk_block_field_idx(parent);
    scan_fsize = parent->header.scan_fsize;
    kk_assert_internal(i < scan_fsize);
    // go through children of the parent
    do {
      kk_block_t* child = kk_block_field_should_free(parent, i, ctx);
      i++;
      if (child != NULL) {
        if (i < scan_fsize) {
          // save our progress to continue here later
          kk_block_field_idx_set(parent,i);
        }
        else {
          // the last field; free the block and move the parent one up
          b = parent;
          parent = _kk_box_ptr(kk_block_field(parent, 0)); // use low-level box as it can be NULL
          kk_block_free(b,ctx);  // note: cannot be a raw block as those have no scanfield
        }
        // and continue with the child
        b = child;
        goto movedown;        
      }
    } while (i < scan_fsize);
    // done: free the block and move up further
    b = parent;
    parent = _kk_box_ptr( kk_block_field(parent, 0) );  // low-level box as it can be NULL
    kk_block_free(b,ctx);  // note: cannot be a raw block as those have no scanfield
  }
  // done
}
#endif

//-----------------------------------------------------------------------------------------
// Mark a block and all children recursively as thread shared
// For marking the recursive algorithm is about twice as fast as the stackless one
// unfortunately so we use recursion first and then switch to stackless.
//-----------------------------------------------------------------------------------------

#define MAX_RECURSE_DEPTH (100)

// Stackless marking is more expensive so we switch to this only after recursing first.
static kk_decl_noinline void kk_block_mark_shared_recx(kk_block_t* b, kk_context_t* ctx);


// Check if a field `i` in a block `b` should be marked, i.e. it is heap allocated and not yet thread shared.
// Optimizes by already marking leaf blocks that have no scan fields.
static inline kk_block_t* kk_block_field_should_mark(kk_block_t* b, kk_ssize_t field, kk_context_t* ctx)
{
  kk_unused(ctx);
  kk_box_t v = kk_block_field(b, field);
  if (kk_box_is_non_null_ptr(v)) {
    kk_block_t* child = kk_ptr_unbox(v,ctx);
    if (!kk_block_is_thread_shared(child)) {
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
static kk_decl_noinline void kk_block_mark_shared_rec(kk_block_t* b, const kk_ssize_t depth, kk_context_t* ctx) 
{
  while(true) {
    if (kk_block_is_thread_shared(b)) {
      return;
    } 
    
    kk_ssize_t scan_fsize = b->header.scan_fsize;
    kk_assert_internal(scan_fsize > 0);
    if (scan_fsize == 1) {
      // if just one field, we can recursively scan without using stack space
      kk_block_make_shared(b);
      kk_block_t* child = kk_block_field_should_mark(b, 0, ctx);
      if (child != NULL) {
        // try to mark the child now
        b = child;
        continue; // tailcall
      }
      return;
    }
    else if (scan_fsize == 2 && !kk_box_is_non_null_ptr(kk_block_field(b, 0))) {
      // optimized code for lists/nodes with boxed first element
      kk_block_make_shared(b);
      kk_block_t* child = kk_block_field_should_mark(b, 1, ctx);
      if (child != NULL) {
        b = child;
        continue; // tailcall
      }
      return;
    }
    else {
      // more than 1 field
      if (depth < MAX_RECURSE_DEPTH) {
        kk_block_make_shared(b);
        kk_ssize_t i = 0;
        if kk_unlikely(scan_fsize >= KK_SCAN_FSIZE_MAX) { 
          scan_fsize = (kk_ssize_t)kk_intf_unbox(kk_block_field(b, 0)); 
          i++;  // skip scan field
        }
        // mark fields up to the last one
        for (; i < (scan_fsize-1); i++) {
          kk_block_t* child = kk_block_field_should_mark(b, i, ctx);
          if (child != NULL) {
            kk_block_mark_shared_rec(child, depth+1, ctx); // recurse with increased depth
          }          
        }
        // and recurse into the last one
        kk_block_t* child = kk_block_field_should_mark(b, i, ctx);
        if (child != NULL) {
          b = child;
          scan_fsize = b->header.scan_fsize;
          continue; // tailcall          
        }
        return;
      }
      else {
        // max recursion depth reached: switch to stackless marking
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

// Unfortunately, we cannot use the _field_idx as the index for marking
// as we also use it for tail recursion context paths. Such TRMC structure may
// be captured under a lambda (by yielding for example) and that might become
// thread shared and we cannot overwrite such indices. 
// (This is unlike freeing where we can use it as we are freeing it anyways)
// So, we steal 8 bits of an unshared reference count. If the reference count
// is too large we just set it to RC_STUCK when it gets marked.
#define KK_RC_MARK_MAX  KK_I32(0x007FFFFF)

static void kk_block_mark_idx_prepare(kk_block_t* b) {
  kk_refcount_t rc = kk_block_refcount(b);
  kk_assert_internal(!kk_refcount_is_thread_shared(rc)); 
  if (rc > KK_RC_MARK_MAX) { rc = KK_RC_MARK_MAX; }   // if rc is too large, cap it
  rc = kk_shl32(rc,8);                                // make room for 8-bit mark index
  kk_assert_internal(rc>=0);
  kk_assert_internal((rc & 0xFF) == 0);  
  kk_block_refcount_set(b, rc);
}

static void kk_block_mark_idx_done(kk_block_t* b) {
  kk_refcount_t rc = kk_block_refcount(b);
  kk_assert_internal(!kk_refcount_is_thread_shared(rc));
  rc = kk_shr32(rc, 8);
  if (rc >= KK_RC_MARK_MAX) { rc = INT32_MAX; }  // ensure it will become stuck if it was too large to contain an index
  kk_block_refcount_set(b, rc);
}

static void kk_block_mark_idx_set(kk_block_t* b, uint8_t i) {
  kk_refcount_t rc = kk_block_refcount(b);
  kk_assert_internal(!kk_refcount_is_thread_shared(rc));
  rc = ((rc & ~0xFF) | i);
  kk_block_refcount_set(b, rc);
}

static uint8_t kk_block_mark_idx(kk_block_t* b) {
  kk_refcount_t rc = kk_block_refcount(b);
  kk_assert_internal(!kk_refcount_is_thread_shared(rc));
  return (uint8_t)rc;
}

// Stackless marking by using pointer reversal
static kk_decl_noinline void kk_block_mark_shared_recx(kk_block_t* b, kk_context_t* ctx) 
{
  fprintf(stderr, "mark shared recx\n");
  kk_block_t* parent = NULL;
  if (kk_block_is_thread_shared(b)) return;
  if (b->header.scan_fsize == 0) return;
  uint8_t i;
  uint8_t scan_fsize;

recurse:
  kk_assert_internal(!kk_block_is_thread_shared(b));  // due to kk_block_field_should_mark
  scan_fsize = b->header.scan_fsize;
  if (scan_fsize == KK_SCAN_FSIZE_MAX) {
    // stack recurse over the stack for large objects (vectors)
    kk_block_mark_shared_recx_large(b, ctx);
  }
  else {
    i = 0;
    kk_block_mark_idx_prepare(b);

    // ---- marking fields starting at field `i` upto `scan_fsize` -----
markfields:
    kk_assert_internal(scan_fsize > 0);
    kk_assert_internal(i <= scan_fsize);
    while (i < scan_fsize) {
      kk_block_t* child = kk_block_field_should_mark(b, i, ctx);
      i++;
      if (child != NULL) {
        // visit the child, but remember our state and link back to the parent
        // note: we cannot optimize for the last child as in freeing as we need to restore all parent fields
        kk_block_field_set(b, i - 1, kk_box_from_potential_null_ptr(parent,ctx));  // low-level box as parent can be NULL
        kk_block_mark_idx_set(b, i);
        parent = b;
        b = child;
        goto recurse;
      }
    }
    kk_block_mark_idx_done(b);
    kk_block_make_shared(b);
  }

  //--- moving back up along the parent chain ------------------
  if (parent != NULL) {
    // move up
    i = kk_block_mark_idx(parent);
    scan_fsize = parent->header.scan_fsize;
    kk_assert_internal(i > 0 && i <= scan_fsize);    
    kk_block_t* pparent = kk_box_to_potential_null_ptr( kk_block_field(parent, i-1), ctx );  // low-level unbox on parent
    kk_block_field_set(parent, i-1, kk_ptr_box(b,ctx));                           // restore original pointer
    b = parent;
    parent = pparent;
    kk_assert_internal(!kk_block_is_thread_shared(b));
    // mark the rest of the fields starting at `i` upto `scan_fsize`
    goto markfields;    
  }

  // done
}


kk_decl_export void kk_block_mark_shared( kk_block_t* b, kk_context_t* ctx ) {
  if (!kk_block_is_thread_shared(b)) {
    if (b->header.scan_fsize == 0) {
      kk_block_make_shared(b); // no scan fields
    }
    else {
      kk_block_mark_shared_rec(b, 0, ctx);
    }
  }
}

kk_decl_export void kk_box_mark_shared( kk_box_t b, kk_context_t* ctx ) {
  if (kk_box_is_non_null_ptr(b)) {
    kk_block_mark_shared( kk_ptr_unbox(b,ctx), ctx );
  }
}


kk_decl_export void kk_box_mark_shared_recx(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_non_null_ptr(b)) {
    kk_block_mark_shared_recx(kk_ptr_unbox(b, ctx), ctx);
  }
}

/*--------------------------------------------------------------------------------------
  TRMC: copy a context following the context path indicated in the _field_idx.
--------------------------------------------------------------------------------------*/

#if defined(KK_HAS_MALLOC_COPY)
static kk_block_t* kk_block_alloc_copy( kk_block_t* b, kk_context_t* ctx ) {
  kk_block_t* c = (kk_block_t*)kk_malloc_copy(b,ctx);
  kk_block_refcount_set(c,0);
  for( kk_ssize_t i = 0; i < kk_block_scan_fsize(b); i++) {
    kk_box_dup(kk_block_field(c, i), ctx);
  }
  return c;
}
#endif

#if !defined(KK_CTAIL_NO_CONTEXT_PATH)
kk_decl_export kk_decl_noinline kk_box_t kk_ctail_context_copy_compose( kk_box_t res, kk_box_t child, kk_context_t* ctx) {
  kk_assert_internal(!kk_block_is_unique(kk_ptr_unbox(res, ctx)));
  kk_box_t  cres = kk_box_null();     // copied result context
  kk_box_t* next = NULL;              // pointer to the context path field in the parent block
  for( kk_box_t cur = res; kk_box_is_ptr(cur); cur = *next ) {
    kk_block_t* b = kk_ptr_unbox(cur, ctx);
    const kk_ssize_t field = kk_block_field_idx(b) - 1;
    kk_assert_internal(field >= 0);
    kk_block_t* c = kk_block_alloc_copy(b,ctx);
    if (next == NULL) { 
      cres = kk_ptr_box(c, ctx);
    }
    else { 
      kk_box_drop(*next,ctx);
      *next = kk_ptr_box(c, ctx);
    }    
    next = kk_block_field_address(c,field);
  }
  kk_assert_internal(next != NULL);
  *next = child;
  kk_box_drop(res,ctx);
  return cres;
}
#endif
