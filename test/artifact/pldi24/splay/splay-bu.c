// 2023, Daan Leijen.
// Sleator and Tarjan's bottom up splay algorithm using parent pointers. (Section 4, page 666)
//
// "Self-Adjusting Binary Search Trees", Daniel Dominic Sleator and Robert Endre Tarjan.
// Journal of the Association for Computing Machinery. Vol. 32, No. 3, July 1985, pp. 652-686.

#define ADDPARENT 1
#include "tree.h"

static tree_t* p(tree_t* x) { return x->parent; }
static tree_t* g(tree_t* x) { return p(p(x)); }

static void rotate_left(tree_t* y) {
  tree_t* x = y->right; tree_t* z = p(y);
  if(z) {
    if (z->left == y) {
      z->left = x;
    } else {
      z->right = x;
    }
  }
  y->right = x->left;
  x->left = y;
  x->parent = z;
  y->parent = x;

  if(y->right) y->right->parent = y;
}

static void rotate_right(tree_t* y) {
  tree_t* x = y->left; tree_t* z = p(y);
  if(z) {
    if (z->right == y) {
      z->right = x;
    } else {
      z->left = x;
    }
  }
  y->left = x->right;
  x->right = y;
  x->parent = z;
  y->parent = x;

  if(y->left) y->left->parent = y;
}

static void splay(tree_t* x) {
  while (p(x)) {
    if (x == p(x)->left) {
      if (g(x) == NULL) { rotate_right(p(x)); }
      else if (p(x) == g(x)->left) { rotate_right(g(x)); rotate_right(p(x)); }
      else { rotate_right(p(x)); rotate_left(p(x)); }
    } else {
      if (g(x) == NULL) { rotate_left(p(x)); }
      else if (p(x) == g(x)->right) { rotate_left(g(x)); rotate_left(p(x)); }
      else { rotate_left(p(x)); rotate_right(p(x)); }
    }
  }
}

static tree_t* access(tree_t* t, tkey_t k) {
  tree_t* p = NULL;
  
  // search
  while (is_node(t)) {
    if (t->key < k) { p = t; t = t->right; }
    else if (t->key > k) { p = t; t = t->left; }
    else break; 
  }
  
  // create if needed
  if (is_leaf(t)) {
    t = node(leaf(),k,leaf());
    t->parent = p;
  }
  
  // link to parent
  if (!p) { return t; }
  else if (p->key < t->key) p->right = t;
  else p->left = t;

  // splay up  
  splay(t);

  return t;
}
  
