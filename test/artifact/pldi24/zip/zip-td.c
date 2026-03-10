// 2023, Daan Leijen
//
// Algorithm 2 in the ziptree paper, adapted to combine with search
//
// Robert E Tarjan, Caleb Levy, and Stephen Timmel. 2021. "Zip Trees". 
// ACM Transactions on Algorithms (TALG) 17 (4). ACM New York, NY: 1--12.

#include "ziptree.h"

static ztree_t* singleton( rank_t rank, tkey_t key ) {
  ztree_t* t = node(leaf(),key,leaf());
  t->rank = rank;
  return t;
}

#if 1
static ztree_t* access(ztree_t* t, tkey_t key) {
  rank_t rank = rank_of(key);  
  ztree_t* root = t;

  // search
  ztree_t* cur = root;
  ztree_t* prev = NULL;
  while (cur != NULL && 
         (rank < cur->rank || (rank == cur->rank && key > cur->key)))
  {
     prev = cur;
     cur = (key < cur->key ? cur->left : cur->right);
  }
  if (cur != NULL && cur->key == key) { return root; }  // extra line: found the key already

  // insert a new node (append)
  ztree_t* x = singleton(rank,key);
  if (cur == root) { root = x; }
  else if (key < prev->key) { prev->left = x; }
  else { prev->right = x; }

  
  // unzip
#if 1 // as published    
  // prepare unzip
  if (cur == NULL) { return root; }
  if (key < cur->key) { x->right = cur; } else { x->left = cur; }
  prev = x;
  while (cur != NULL) {
    ztree_t* fix = prev;
    if (cur->key < key) {
      do {
        prev = cur;
        cur = cur->right;
      } while (!(cur == NULL || cur->key > key));
    }
    else {
      assert(cur->key != key);
      do {
        prev = cur;
        cur = cur->left;
      } while (!(cur == NULL || cur->key < key));
    }
    if (fix->key > key || (fix == x && prev->key > key)) {
      fix->left = cur;
    }
    else {
      fix->right = cur;
    }
  }
#elif AS_FUNCTIONAL1 // no inner loop
  ztree_t** accl = &x->left;
  ztree_t** accr = &x->right;
  while (cur != NULL) {    
    if (cur->key < key) {
      *accl = cur;
      accl = &cur->right;
      cur = cur->right;      
    }
    else {
      assert(cur->key != key);
      *accr = cur;
      accr = &cur->left;
      cur = cur->left;
    }
  }
  *accl = NULL;
  *accr = NULL;    
#elif AS_FUNCTIONAL   // the functional one with inner loop
  ztree_t** accl = &x->left;
  ztree_t** accr = &x->right;
  while (cur != NULL) {    
    if (cur->key < key) {
      *accl = cur;
      do {
        accl = &cur->right;
        cur = cur->right;
      } while (!(cur == NULL || cur->key > key));
    }
    else {
      assert(cur->key != key);
      *accr = cur;
      do {
        accr = &cur->left;
        cur = cur->left;
      } while (!(cur == NULL || cur->key < key));
    }
  }
  *accl = NULL;
  *accr = NULL;  
#else // functional with one unrolling as Tarjan's version.
  // prepare unzip
  if (cur == NULL) { return root; }
  if (key < cur->key) { x->right = cur; } else { x->left = cur; }
  
  ztree_t** accl = &x->left;
  ztree_t** accr = &x->right;
  while (cur != NULL) {    
    if (cur->key < key) {
      do {
        accl = &cur->right;
        cur = cur->right;
      } while (!(cur == NULL || cur->key > key));
      *accr = cur;
    }
    else {
      assert(cur->key != key);
      do {
        accr = &cur->left;
        cur = cur->left;
      } while (!(cur == NULL || cur->key < key));
      *accl = cur;
    }    
  }
  // *accl = NULL;
  // *accr = NULL;
#endif    
  return root;
}

#else  // as proven in Iris (fip_td.v)


typedef ztree_t ziptree_t;

static bool is_higher_rank( rank_t r1, tkey_t k1, rank_t r2, tkey_t k2 ) {
  return (r1 > r2 || (r1 == r2 && k1 < k2));
}

static ziptree_t* access(ziptree_t* root, tkey_t key) 
{
  const rank_t rank = rank_of(key);  

  // find
  ziptree_t** prev = &root;
  ziptree_t*  cur  = root;
  while (cur != NULL && is_higher_rank( cur->rank, cur->key, rank, key ))  
  {
     if (key < cur->key) {
       prev = &cur->left; cur = cur->left;
     } 
     else {
       prev = &cur->right; cur = cur->right;
     }
  }
  if (cur != NULL && cur->key == key) { return root; }  // found the key already

  // allocate new node
  ziptree_t* x = singleton(rank,key);
  x->left = x->right = cur;

  // unzip
  ziptree_t** accl = &x->left;
  ziptree_t** accr = &x->right;
  while (cur != NULL) {
    if (cur->key < key) {
      do {
        accl = &cur->right;
        cur = cur->right;
      } while (cur != NULL && cur->key < key);
      *accr = cur;
    }
    else {
      do {
        accr = &cur->left;
        cur = cur->left;
      } while (cur != NULL && cur->key > key);
      *accl = cur;
    }
  }
  
  *prev = x;  
  return root;
}

#endif