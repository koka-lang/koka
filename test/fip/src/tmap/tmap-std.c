#include <stdio.h>
#include <stdlib.h>
#define MAX(a,b) (((a)>(b))?(a):(b))

struct node {
    int32_t data;
    struct node* left;
    struct node* right;
};

struct node* create_node(int32_t data) {
    struct node* new_node = (struct node*)malloc(sizeof(struct node));
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

void tmap(struct node* root, int32_t (*f)(int32_t), struct node** dest) {
    while(root != NULL) {
        struct node* root_ = create_node(root->data);
        tmap(root->left, f, &root_->left);
        root_->data = f(root_->data);
        *dest = root_;
        dest = &root_->right;
        root = root->right;
    }
    *dest = NULL;
}

int increment(int x){
    return x+1;
}

void test(int n) {
    struct node* xs = insert_range(1, n);
    int iter = 100000000 / MAX(n, 1);
    int32_t x = 0;

    for(int i = 0; i < iter; i++) {
        struct node* ys;
        tmap(xs, increment, &ys);
        x += sum_tree(ys);
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