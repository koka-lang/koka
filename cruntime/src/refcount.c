#include "runtime.h"

/*--------------------------------------------------------------------------------------
  Decrementing reference counts
  When freeing a block, we need to decrease reference counts of its children
  recursively. We carefully optimize to use no stack space in case of single field
  chains (like lists) and recurse to limited depth in other cases, using a 
  `delayed_free` list in the thread local data. 
--------------------------------------------------------------------------------------*/

static bool block_decref_no_free(block_t* b) {
  uint32_t count = (b->header.rc32.lo)--;
#if REFCOUNT_LIMIT_TO_32BIT
  if (count == 0) return true;
#else
  if (count==0) {
    if (likely(b->header.rc32.hi == 0)) return true;
    b->header.rc32.hi--; // propagate borrow.
  }
#endif
  return false;
}



static void block_push_delayed_free(block_t* b) {
  assert(b->header.h.refcount == 0);
  block_t* delayed = tld->delayed_free;
  // encode the next pointer into the block header
  b->header.rc32.lo = (uint32_t)((uintptr_t)delayed);
#if (INTPTR_SIZE > 4)
  b->header.h.tag = (uint16_t)(sar((intptr_t)delayed,32));
  assert(sar((intptr_t)delayed,48) == 0 || sar((intptr_t)delayed, 48) == -1);
#endif
  tld->delayed_free = b;
}

static noinline void block_decref_free(block_t* b, size_t depth);

static void block_decref_delayed() {
  block_t* delayed; 
  while ((delayed = tld->delayed_free) != NULL) {
    tld->delayed_free = NULL;
    do {
      block_t* b = delayed;
      // decode the next element in the delayed list from the block header
      intptr_t next = (intptr_t)b->header.rc32.lo;
#if (INTPTR_SIZE>4)
      next += ((intptr_t)((int16_t)(b->header.h.tag)) << 32); // sign extended
#endif
#ifndef NDEBUG
      b->header.rc32.lo = UINT32_MAX;
#endif
      delayed = (block_t*)next;
      // and free the block
      block_decref_free(b, 0);
    } while (delayed != NULL);
  }
}

#define MAX_RECURSE_DEPTH (100)

static noinline void block_decref_free(block_t* b, size_t depth) {
  while(true) {
    assert(b->header.rc32.lo == UINT32_MAX);
    size_t scan_fsize = b->header.h.scan_fsize;
    if (scan_fsize == 0) {
      // nothing to scan, just free
      runtime_free(b);
      return;
    }
    else if (scan_fsize == 1) {
      // if just one field, we can recursively free without using stack space
      const box_t v = b->fields[0]; 
      runtime_free(b);
      if (is_ptr(v)) {
        // try to free the child now
        b = ptr_block(unbox_ptr(v));
        if (block_decref_no_free(b)) {
          // continue freeing on this block
          continue; // tailcall
        }
      }
      return; 
    }
    else {
      // more than 1 field
      if (unlikely(scan_fsize >= SCAN_FSIZE_MAX)) { scan_fsize = unbox_enum(b->fields[0]); }
      if (depth < MAX_RECURSE_DEPTH) {
        // free fields up to the last one
        for (size_t i = 0; i < (scan_fsize-1); i++) {
          box_t v = b->fields[i];
          if (is_ptr(v)) {
            block_t* vb = ptr_block(unbox_ptr(v));
            if (block_decref_no_free(vb)) {
              block_decref_free(vb, depth+1); // recurse with increased depth
            }
          }
        }
        // and recurse into the last one
        box_t v = b->fields[scan_fsize - 1];
        runtime_free(b);
        if (is_ptr(v)) {
          b = ptr_block(unbox_ptr(v));
          if (block_decref_no_free(b)) {
            continue; // tailcall
          }
        }
        return;
      }
      else {
        // recursed too deep, push this block onto the todo list
        block_push_delayed_free(b);
        return;
      }
    }
  }
}

void block_free(block_t* b) {
  assert(b->header.rc32.lo == UINT32_MAX);
  if (b->header.h.scan_fsize==0) {
    runtime_free(b); // deallocate directly if nothing to scan
  }
  else {
    block_decref_free(b, 0);
    block_decref_delayed();
  }
}

void noinline ptr_check_free(ptr_t p) {
  block_t* b = ptr_block(p);
  assert(b->header.rc32.lo == UINT32_MAX);  // just underflowed
  if (b->header.rc32.hi==0) {
    block_free(b);
  }
  else {
    // large refcount, propagate the borrow
    b->header.rc32.hi--;
  }
}

