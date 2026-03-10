// 2023, Daan Leijen
// Sleator and Tarjan's top-down splay algorithm as published (Section 4, page 669)
//
// "Self-Adjusting Binary Search Trees", Daniel Dominic Sleator and Robert Endre Tarjan.
// Journal of the Association for Computing Machinery. Vol. 32, No. 3, July 1985, pp. 652-686.

#include "tree.h"


#define LINK_LEFT \
  l->right = t; l = t; t = t->right;

#define LINK_RIGHT \
  r->left = t; r = t; t = t->left;

#define ROTATE_LEFT \
  { tree_t* tr = t->right; t->right = tr->left; tr->left = t; t = tr; }

#define ROTATE_RIGHT \
  { tree_t* tl = t->left; t->left = tl->right; tl->right = t; t = tl; }

#define ASSEMBLE \
  l->right = t->left; \
  r->left = t->right; \
  t->right  = null.left; \
  t->left = null.right;

#define ENSURE(x) \
  if ((x)==NULL) { x = node(leaf(),k,leaf()); }

static tree_t* access( tree_t* t, tkey_t k ) {
  tree_t null;
  null.left = leaf();
  null.right = leaf();
  tree_t* l = &null; 
  tree_t* r = &null; 
  while(true) {
    ENSURE(t);
    if (k < t->key) {
      ENSURE(t->left);
      if (k == t->left->key)      { LINK_RIGHT; break; }
      else if (k < t->left->key)  { ROTATE_RIGHT; LINK_RIGHT; }
      else                        { LINK_RIGHT; LINK_LEFT; }      
    }
    else if (k > t->key) {
      ENSURE(t->right);
      if (k == t->right->key)     { LINK_LEFT; break; }
      else if (k > t->right->key) { ROTATE_LEFT; LINK_LEFT; }
      else                        { LINK_LEFT; LINK_RIGHT; }
    }
    else break;
  }
  ASSEMBLE;
  return t;
}

