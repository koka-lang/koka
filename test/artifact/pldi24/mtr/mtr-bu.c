// 2023, Daan Leijen.
//
// Allen and Munro bottom-up move-to-root using parent pointers.
//
// "Self-Organizing Binary Search Trees", Brian Allen and Ian Munro.
// Journal of the Association for Computing Machinery. Vol 25, No 4. October 1978. pp 526--535

#define ADDPARENT 1
#include "tree.h"

static void left_rotate( tree_t** root, tree_t* t) {
  tree_t* tr = t->right;
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
  
static void right_rotate(tree_t** root, tree_t* t) {
  tree_t* tl = t->left;
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
  
  
static void mtr(tree_t** root, tree_t* t) {
  while (t->parent) {
    if (t->parent->left == t) {
      right_rotate(root,t->parent);
    }
    else {
      left_rotate(root,t->parent);
    }
  }
}
  
static tree_t* access(tree_t* t, tkey_t k) {
  tree_t* root = t;
  tree_t* p = NULL;
  
  // search
  while (t) {
    if (t->key < k) { p = t; t = t->right; }
    else if (t->key > k) { p = t; t = t->left; }
    else break; 
  }
  
  // create if needed
  if (!t) {
    t = node(leaf(),k,leaf());
    t->parent = p;
  }
  
  // link to parent
  if (!p) root = t;
  else if (p->key < t->key) p->right = t;
  else p->left = t;  

  // move to root
  mtr(&root,t);

  return root;
}
  
