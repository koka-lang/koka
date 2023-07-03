#include <stdio.h>
#include <stdlib.h>
#define MAX(a,b) (((a)>(b))?(a):(b))

struct node {
    int32_t header;
    int32_t data;
    struct node* left;
    struct node* right;
};

struct node* create_node(int32_t data) {
    struct node* new_node = (struct node*)malloc(sizeof(struct node));
    new_node->header = 0;
    new_node->data = data;
    new_node->left = NULL;
    new_node->right = NULL;
    return new_node;
}

struct node* insert_range(int32_t start, int32_t end) {
    if (start > end) return NULL;

    int32_t mid = start + (end - start) / 2;
    struct node* root = create_node(mid);

    root->left = insert_range(start, mid - 1);
    root->right = insert_range(mid + 1, end);

    return root;
}

int sum_tree(struct node* root) {
    if (root == NULL) return 0;

    int n = root->data + sum_tree(root->left) + sum_tree(root->right);
    free(root);
    return n;
}

struct node* tmap(struct node* root, int32_t (*f)(int32_t)) {
    struct node* acc = NULL;

    acc: 
        while(root != NULL) {
            struct node* acc_ = create_node(root->data);
            acc_->left = acc;
            acc_->right = root->right;
            root = root->left;
            acc = acc_;
        }
    
    app:
        if(acc == NULL) return root;
        if(acc->header == 0) {
            struct node* right = acc->right;
            acc->header = 1;
            acc->data = f(acc->data);
            acc->right = acc->left;
            acc->left = root;
            root = right;
            goto acc;
        } else { // acc->header == 1
            struct node* acc_ = acc->right;
            acc->right = root;
            root = acc;
            acc = acc_;
            goto app;
        }
}

int increment(int x){
    return x+1;
}

void test(int n) {
    struct node* xs = insert_range(1, n);
    int iter = 100000000 / MAX(n, 1);
    int32_t x = 0;

    for(int i = 0; i < iter; i++) {
        x += sum_tree(tmap(xs, increment));
    }
    sum_tree(xs); // free xs

    printf("total: %d\n", x);
}

int main(int argc, char* argv[]) {
    int n;
    if (argc < 2) {
        printf("Please provide a natural number as an argument.\n");
        return 0;
    } else {
        n = atoi(argv[1]);
    }

    test(n);

    return 0;
}