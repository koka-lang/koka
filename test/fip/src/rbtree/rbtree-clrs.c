// Red-black tree insertion as in 'Introduction to Algorithms', Cormen, Leiserson, Rivest, Stein

#include <stdio.h>
#include <stdlib.h>
#define MAX(a,b) (((a)>(b))?(a):(b))

enum Color { RED, BLACK };
enum Bool { TRUE, FALSE };

typedef struct Node {
    int32_t key;
    enum Bool value;
    enum Color color;
    struct Node *left;
    struct Node *right;
    struct Node *parent;
} Node;

typedef struct RedBlackTree {
    Node *nil;
    Node *root;
} RedBlackTree;

void left_rotate(RedBlackTree *T, Node *x) {
    Node *y = x->right;
    x->right = y->left;
    if (y->left != T->nil) {
        y->left->parent = x;
    }
    y->parent = x->parent;
    if (x->parent == T->nil) {
        T->root = y;
    } else if (x == x->parent->left) {
        x->parent->left = y;
    } else {
        x->parent->right = y;
    }
    y->left = x;
    x->parent = y;
}

void right_rotate(RedBlackTree *T, Node *x) {
    Node *y = x->left;
    x->left = y->right;
    if (y->right != T->nil) {
        y->right->parent = x;
    }
    y->parent = x->parent;
    if (x->parent == T->nil) {
        T->root = y;
    } else if (x == x->parent->right) {
        x->parent->right = y;
    } else {
        x->parent->left = y;
    }
    y->right = x;
    x->parent = y;
}

void insert_fixup(RedBlackTree *T, Node *z) {
    while (z->parent->color == RED) {
        if (z->parent == z->parent->parent->left) {
            Node *y = z->parent->parent->right;
            if (y->color == RED) {
                z->parent->color = BLACK;
                y->color = BLACK;
                z->parent->parent->color = RED;
                z = z->parent->parent;
            } else {
                if (z == z->parent->right) {
                    z = z->parent;
                    left_rotate(T, z);
                }
                z->parent->color = BLACK;
                z->parent->parent->color = RED;
                right_rotate(T, z->parent->parent);
            }
        } else {
            Node *y = z->parent->parent->left;
            if (y->color == RED) {
                z->parent->color = BLACK;
                y->color = BLACK;
                z->parent->parent->color = RED;
                z = z->parent->parent;
            } else {
                if (z == z->parent->left) {
                    z = z->parent;
                    right_rotate(T, z);
                }
                z->parent->color = BLACK;
                z->parent->parent->color = RED;
                left_rotate(T, z->parent->parent);
            }
        }
    }
    T->root->color = BLACK;
}

void insert(RedBlackTree *T, int32_t key, enum Bool value) {
    Node *z = (Node *)malloc(sizeof(Node));
    z->key = key;
    z->value = value;
    Node *y = T->nil;
    Node *x = T->root;
    while (x != T->nil) {
        y = x;
        if (z->key < x->key) {
            x = x->left;
        } else {
            x = x->right;
        }
    }
    z->parent = y;
    if (y == T->nil) {
        T->root = z;
    } else if (z->key < y->key) {
        y->left = z;
    } else {
        y->right = z;
    }
    z->left = T->nil;
    z->right = T->nil;
    z->color = RED;
    insert_fixup(T, z);
}

RedBlackTree *empty_rbtree() {
    Node *nil = (Node *)malloc(sizeof(Node));
    nil->color = BLACK;
    RedBlackTree *t = (RedBlackTree *)malloc(sizeof(RedBlackTree));
    t->root = nil;
    t->nil = nil;
    return t;
}

int fold(Node* nil, Node *t, int32_t b, int32_t(*f)(int32_t, enum Bool, int)) {
    if (t == nil) {
        return b;
    }
    int32_t left = fold(nil, t->left, b, f);
    int32_t right = fold(nil, t->right, f(t->key, t->value, left), f);
    free(t);
    return right;
}

void make_tree_aux(int32_t n, RedBlackTree *t) {
    if (n <= 0) return;
    
    int32_t n1 = n - 1;
    insert(t, n1, (n1 % 10 == 0) ? TRUE : FALSE);
    make_tree_aux(n1, t);
}

RedBlackTree *make_tree(int32_t n) {
    RedBlackTree *t = empty_rbtree();
    make_tree_aux(n, t);
    return t;
}

int increment(int32_t k, enum Bool v, int32_t r) {
  if(v == TRUE) { return r + 1; } else { return r; } 
}

void test(int n) {
    int iter = 10000000 / MAX(n, 1);
    int32_t acc = 0;
    for(int i = 0; i < iter; i++) {
        RedBlackTree *t = make_tree(n);
        acc += fold(t->nil, t->root, 0, increment);
        free(t->nil);
        free(t);
    }
    printf("total: %d\n", acc);
}

int main(int argc, char *argv[]) {
    int n = 100;
    if (argc > 1) {
        n = atoi(argv[1]);
    }
    test(n);
    return 0;
}