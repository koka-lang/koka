// 2023, Daan Leijen.
//
// Direct implementation of Stephenson's algorithm, page 27, figure (C).
//
// "A Method for Constructing Binary Search Trees by Making Insertions at the Root", C.J.Stephenson.
// International Journal of Computer and Information Sciences, VoL 9, No. 1, 1980.

#include "tree.h"

static tree_t* access( tree_t* t, tkey_t k ) {
  tree_t dummy;
  dummy.left = leaf();
  dummy.right = leaf();
  tree_t** accl = &dummy.left;
  tree_t** accr = &dummy.right;
  while (true) {
    if (is_node(t)) {
      tree_t* l = t->left;
      tree_t* r = t->right;
      if (t->key < k) {
        *accl = t; 
        t->right = NULL;       
        accl = &t->right;        
        t = r;
      }
      else if (t->key > k) {
        *accr = t;
        t->left = NULL;
        accr = &t->left;
        t = l;
      }
      else {
        *accl = l;
        *accr = r;
        t->left = dummy.left;
        t->right = dummy.right;
        return t;
      } 
    }
    else {
      *accl = leaf();
      *accr = leaf();
      return node(dummy.left, k, dummy.right);
    }
  }
}


