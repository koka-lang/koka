// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by Jeremy Zerfas
// Based on the C++ program from Jon Harrop, Alex Mizrahi, and Bruno Coutinho.
// *reset*

// This controls the width of lines that are output by this program.
#define MAXIMUM_LINE_WIDTH  60

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef off_t off64_t; // This is needed to keep APR happy on 32 bit systems.
#include <apr_pools.h>

// intptr_t should be the native integer type on most sane systems.
typedef intptr_t intnative_t;

typedef struct tree_node{
   struct tree_node   * left_Node, * right_Node;
} tree_node;


// Create a binary tree of depth tree_Depth in memory_Pool, set the root node's
// value to root_Node_Value, and finally return a pointer to the created binary
// tree.
static inline tree_node * create_Tree(const intnative_t tree_Depth, 
  apr_pool_t * const memory_Pool){
   tree_node * const root_Node=apr_palloc(memory_Pool, sizeof(tree_node));

   // If tree_Depth is one or more then recursively call create_Tree() in order
   // to create the left and right subtrees using 2*root_Node_Value-1 and
   // 2*root_Node_Value respectively as the root values for those subtrees.
   if(tree_Depth>0){
      root_Node->left_Node=create_Tree(tree_Depth-1, memory_Pool);
      root_Node->right_Node=create_Tree(tree_Depth-1, memory_Pool);
   }else
      root_Node->left_Node=root_Node->right_Node=NULL;

   return root_Node;
}


// Compute and return the checksum for the binary tree that has root_Node as the
// root node.
static inline intnative_t compute_Tree_Checksum(
  const tree_node * const root_Node){
   // If there are subtrees then recursively call compute_Tree_Checksum() on
   // them and factor their values into the checksum, otherwise just return
   // the value of root_Node.
   if(root_Node->left_Node)
      return compute_Tree_Checksum(root_Node->left_Node)+
        compute_Tree_Checksum(root_Node->right_Node)+1;
   else
      return 1;
}


int main(int argc, char ** argv){
   // Set minimum_Tree_Depth to 4 and maximum_Tree_Depth to the maximum of what
   // was specified as the argument to the program and minimum_Tree_Depth+2.
   const intnative_t minimum_Tree_Depth=4;
   intnative_t maximum_Tree_Depth=atoi(argv[1]);
   if(maximum_Tree_Depth < minimum_Tree_Depth+2)
      maximum_Tree_Depth=minimum_Tree_Depth+2;

   apr_initialize();
   apr_pool_t * memory_Pool;

   // Create a memory pool, create a binary tree of depth maximum_Tree_Depth+1,
   // compute the checksum of the binary tree, print the statistics, and then
   // delete the memory pool.
   apr_pool_create_unmanaged(&memory_Pool);
   tree_node * stretch_Tree=create_Tree(maximum_Tree_Depth+1, memory_Pool);
   printf("stretch tree of depth %jd\t check: %jd\n",
     (intmax_t)maximum_Tree_Depth+1,
     (intmax_t)compute_Tree_Checksum(stretch_Tree));
   apr_pool_destroy(memory_Pool);

   // Create a memory pool and then create a long-lived binary tree of depth
   // maximum_Tree_Depth which will be left alone for a while while
   // more binary trees get allocated and deallocaited as required by the
   // rules. We'll finish working with this later.
   apr_pool_create_unmanaged(&memory_Pool);
   tree_node * long_Lived_Tree=create_Tree(maximum_Tree_Depth, memory_Pool);

   // Create a lot of binary trees in parallel of depths ranging from
   // minimum_Tree_Depth to maximum_Tree_Depth, compute and tally up all their
   // checksums, destroy the trees, and then record the statistics to
   // output_Buffer[] so they can be displayed in order later.
   char output_Buffer[maximum_Tree_Depth+1][MAXIMUM_LINE_WIDTH+1];
   intnative_t current_Tree_Depth;
   #pragma omp parallel for
   for(current_Tree_Depth=minimum_Tree_Depth;
     current_Tree_Depth<=maximum_Tree_Depth; current_Tree_Depth+=2){
      intnative_t iterations=1<<(maximum_Tree_Depth-current_Tree_Depth+
        minimum_Tree_Depth);

      // Create a memory pool for this thread to use.
      apr_pool_t * thread_Memory_Pool;
      apr_pool_create_unmanaged(&thread_Memory_Pool);

      intnative_t i=1, total_Trees_Checksum=0;
      for(; i<=iterations; ++i){
         // Create a binary tree of depth current_Tree_Depth
         tree_node * const tree_1=create_Tree(current_Tree_Depth,
           thread_Memory_Pool);

         total_Trees_Checksum+=compute_Tree_Checksum(tree_1);

         apr_pool_clear(thread_Memory_Pool);
      }

      apr_pool_destroy(thread_Memory_Pool);

      // Record the statistics for the trees of depth current_Tree_Depth.
      sprintf(output_Buffer[current_Tree_Depth],
        "%jd\t trees of depth %jd\t check: %jd\n", (intmax_t)iterations,
        (intmax_t)current_Tree_Depth, (intmax_t)total_Trees_Checksum);
   }

   // Print the statistics for all of the various tree depths.
   for(current_Tree_Depth=minimum_Tree_Depth;
     current_Tree_Depth<=maximum_Tree_Depth; current_Tree_Depth+=2)
      printf("%s", output_Buffer[current_Tree_Depth]);

   // Compute the checksum of the long-lived binary tree that we created
   // earlier, print the statistics, and then delete the memory pool.
   printf("long lived tree of depth %jd\t check: %jd\n",
     (intmax_t)maximum_Tree_Depth,
     (intmax_t)compute_Tree_Checksum(long_Lived_Tree));
   apr_pool_destroy(memory_Pool);

   apr_terminate();
   return 0;
}
