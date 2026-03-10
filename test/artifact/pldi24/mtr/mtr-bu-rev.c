// 2023, Daan Leijen.
//
// Bottom-up move-to-root but using pointer reversal instead of parent pointers.
// We encode the left or right path using the least-significant pointer bit.
//
// "Self-Organizing Binary Search Trees", Brian Allen and Ian Munro.
// Journal of the Association for Computing Machinery. Vol 25, No 4. October 1978. pp 526--535

#include "tree.h"

#include <stdint.h>
#include <stdbool.h>

// encode zipper tag (NodeR, NodeL) into the lowest bit of the upward pointer of the zipper
typedef tree_t zipper_t;

static zipper_t* encode_node_right( tree_t* t ) {
  return (zipper_t*)((uintptr_t)t | 1);
}

static zipper_t* encode_node_left( tree_t* t ) {
  return (zipper_t*)t;
}

static tree_t* decode_node( zipper_t* z ) {
  return (tree_t*)((uintptr_t)z & ~1);
}

static bool is_node_right( zipper_t* z ) {
  return (((uintptr_t)z & 1) == 1);
}

static bool is_node_left( zipper_t* z ) {
  return !is_node_right(z);
}



static tree_t* mtr(zipper_t* z, tree_t* t) {
  while (z) {
    tree_t* tz = decode_node(z);
    if (is_node_left(z)) {
      z = tz->left;         // go up
      tz->left = t->right;
      t->right = tz;
    }
    else {
      z = tz->right;        // go up
      tz->right = t->left;
      t->left = tz;
    }
  }
  return t;
}
  
static tree_t* access(tree_t* t, tkey_t k) {
  tree_t* root = t;
  zipper_t* z = NULL;  

  // search
  while (t) {
    if (t->key < k) { 
      tree_t* r = t->right;
      t->right = z;  // point up
      z = encode_node_right(t);        
      t = r; 
    }
    else if (t->key > k) { 
      tree_t* l = t->left;
      t->left = z;
      z = encode_node_left(t);
      t = l;
    }
    else break; 
  }
  
  // create if needed
  if (!t) {
    t = node(leaf(),k,leaf());
  }
  
  // move to root
  return mtr(z,t);
}
  
