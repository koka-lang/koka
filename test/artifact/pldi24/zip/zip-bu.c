// 2023, Daan Leijen
//
// Algorithm 1 in the ziptree paper, but using parent pointers without
// needing recursion (bottom-up zip)
//
// Robert Endre Tarjan, Caleb Levy, and Stephen Timmel. 2021. "Zip Trees". 
// ACM Transactions on Algorithms (TALG) 17 (4). ACM New York, NY: 1--12.

//#if ADDPARENT  
//#define BAILOUT_EARLY 0
//#endif

#ifndef BAILOUT_EARLY
#define BAILOUT_EARLY 1
#endif

#define ADDPARENT 1

#include "ziptree.h"

static ztree_t* singleton( rank_t rank, tkey_t key ) {
  ztree_t* t = node(leaf(),key,leaf());
  t->rank = rank;
  return t;
}

static void left_rotate( ztree_t** root, ztree_t* t) {
  ztree_t* tr = t->right;
  if (tr) {
    t->right = tr->left;
    if (tr->left) tr->left->parent = t;
    tr->parent = t->parent;
  }
  
  if (!t->parent) *root = tr;
  else if (t == t->parent->left) t->parent->left = tr;
  else t->parent->right = tr;
  if (tr) tr->left = t;
  t->parent = tr;
}
  
static void right_rotate(ztree_t** root, ztree_t* t) {
  ztree_t* tl = t->left;
  if (tl) {
    t->left = tl->right;
    if (tl->right) tl->right->parent = t;
    tl->parent = t->parent;
  }
  if (!t->parent) *root = tl;
  else if (t == t->parent->left) t->parent->left = tl;
  else t->parent->right = tl;
  if (tl) tl->right = t;
  t->parent = tl;
}
  

static bool is_higher_rank( ztree_t* x, ztree_t* y ) {
  return (x->rank > y->rank || (x->rank == y->rank && x->key < y->key));
}

static void unzip(ztree_t** root, ztree_t* t) {
  while (t->parent) { 
    if (is_higher_rank(t,t->parent)) {
      if (t->parent->left == t) {
        right_rotate(root,t->parent);
      }
      else {
        left_rotate(root,t->parent);
      }
    }
    else {
      #if BAILOUT_EARLY
      break;
      #else
      t = t->parent;
      #endif
    }
  }
  if (!t->parent) { *root = t; }
}

static ztree_t* access(ztree_t* t, tkey_t key) {
  rank_t rank = rank_of(key);  
  ztree_t* root = t;

  // traverse down
  ztree_t* parent = NULL;
  while (t != NULL) {
    if (t->key < key) {
      parent = t;
      t = t->right;
    }
    else if (t->key > key) {
      parent = t;
      t = t->left;
    }
    else {
      break;
    }
  }

  // create new node at a leaf position if needed
  if (!t) {
    t = singleton(rank,key);
    t->parent = parent;
  }

  // link to parent
  if (parent==NULL) root = t;
  else if (parent->key < t->key) parent->right = t;
  else parent->left = t;
  
  // unzip up to the insertion point determined by rank  
  unzip(&root,t);  
  return root;
}

