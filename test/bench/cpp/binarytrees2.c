// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by Jeremy Zerfas
// Based on the C++ program from Jon Harrop, Alex Mizrahi, and Bruno Coutinho.

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <apr_pools.h>

// intptr_t should be the native integer type on most sane systems.
typedef intptr_t intnative_t;

typedef struct tree_node{
	struct tree_node * left_Node, * right_Node;
} tree_node;


// Create a binary tree of depth tree_Depth in memory_Pool and return a pointer
// to the created binary tree.
static tree_node * create_Tree(const intnative_t tree_Depth,
  apr_pool_t * const memory_Pool){
	tree_node * const root_Node=apr_pcalloc(memory_Pool, sizeof(tree_node));

	// If tree_Depth is one or more then recursively call create_Tree() in order
	// to create the left and right subtrees.
	if(tree_Depth>0){
		root_Node->left_Node=create_Tree(tree_Depth-1, memory_Pool);
		root_Node->right_Node=create_Tree(tree_Depth-1, memory_Pool);
	}

	return root_Node;
}


// Compute and return the checksum for the binary tree that has root_Node as the
// root node.
static intnative_t compute_Tree_Checksum(const tree_node * const root_Node){
	// If there are subtrees then recursively call compute_Tree_Checksum() on
	// them and return 1 plus the checksum of those subtrees.
	if(root_Node->left_Node && root_Node->right_Node)
		return compute_Tree_Checksum(root_Node->left_Node)+
		  compute_Tree_Checksum(root_Node->right_Node)+1;

	// If the function gets here then this is a single node tree so just return
	// 1 as the checksum.
	return 1;
}


int main(int argc, char *argv[]){
	// Set minimum_Tree_Depth to 4 and maximum_Tree_Depth to the maximum of what
	// was specified as the argument to the program and minimum_Tree_Depth+2.
	const intnative_t minimum_Tree_Depth=4, 
	  maximum_Tree_Depth=atoi(argv[1])<minimum_Tree_Depth+2 ?
	  minimum_Tree_Depth+2 : atoi(argv[1]);

	apr_initialize();

	// Create a memory_Pool which will be used for storing both the stretch_Tree
	// and the long_Lived_Tree.
	apr_pool_t * memory_Pool;
	apr_pool_create_unmanaged(&memory_Pool);

	// Create a stretch_Tree of depth maximum_Tree_Depth+1, compute its
	// checksum, and print its statistics. This work could be done in parallel
	// along with all the other tree processing but APR memory pools aren't
	// quite as streamlined as other memory pool implementations so it uses less
	// resources to do this work by itself and then clear the memory_Pool so
	// that most of the memory that was already allocated for the stretch_Tree
	// can be reused for the upcoming long_Lived_Tree work rather than having
	// APR allocate more memory for memory pools. Unfortunately since the
	// long_Lived_Tree is about half the size of the stretch_Tree, this ends up
	// wasting about half the memory that was being used by the stretch_Tree.
	// APR subpools could be used to use that otherwise wasted memory for the
	// processing of other trees that will be done later but it appears subpools
	// only work with managed pools (even though APR's documentation for the
	// apr_pool_create_unmanaged_ex() function seems to suggest that it possibly
	// should work for unmanaged pools too) which are noticeably slower than
	// unmanaged memory pools.
	tree_node * stretch_Tree=create_Tree(maximum_Tree_Depth+1, memory_Pool);
	printf("stretch tree of depth %jd\t check: %jd\n",
	  (intmax_t)maximum_Tree_Depth+1,
	  (intmax_t)compute_Tree_Checksum(stretch_Tree));
	apr_pool_clear(memory_Pool);

	// The long_Lived_Tree will be created in just a little bit simultaneously
	// (assuming OpenMP was enabled and the program is running on a multi-
	// processor system) while the rest of the trees are also being processed.
	// long_Lived_Tree will store the reference to it which will remain valid
	// until near the end of the program.
	tree_node * long_Lived_Tree;

	// These will be used to store checksums for the various trees so the
	// statistics for the various trees can be output in the correct order
	// later.
	intnative_t long_Lived_Tree_Checksum,
	  tree_Checksums[(maximum_Tree_Depth-minimum_Tree_Depth+2)/2];
	#pragma omp parallel
	{
		// Have one thread create the long_Lived_Tree of depth
		// maximum_Tree_Depth in the memory_Pool which was already previously
		// used for the stretch_Tree, compute the long_Lived_Tree_Checksum, and
		// then just leave the long_Lived_Tree alone for a while while the rest
		// of the binary trees finish processing (which should have
		// simultaneously been started to be processed by any other available
		// threads).
		#pragma omp single nowait
		{
			long_Lived_Tree=create_Tree(maximum_Tree_Depth, memory_Pool);
			long_Lived_Tree_Checksum=compute_Tree_Checksum(long_Lived_Tree);
		}

		// Create a thread_Memory_Pool for this thread to use.
		apr_pool_t * thread_Memory_Pool;
		apr_pool_create_unmanaged(&thread_Memory_Pool);

		#pragma omp for nowait
		for(intnative_t tree_Depth=minimum_Tree_Depth;
		  tree_Depth<=maximum_Tree_Depth; tree_Depth+=2){

			// Create a bunch of binary trees of depth tree_Depth, compute their
			// checksums, and add the checksums to the total_Trees_Checksum.
			intnative_t total_Trees_Checksum=0;
			for(intnative_t iterations=
			  1<<(maximum_Tree_Depth-tree_Depth+minimum_Tree_Depth);
			  iterations-->0;){
				apr_pool_clear(thread_Memory_Pool);
				total_Trees_Checksum+=compute_Tree_Checksum(
				  create_Tree(tree_Depth, thread_Memory_Pool));
			}

			// Record the total_Trees_Checksum for the trees of depth
			// tree_Depth.
			tree_Checksums[(tree_Depth-minimum_Tree_Depth)/2]=
			  total_Trees_Checksum;
		}

		apr_pool_destroy(thread_Memory_Pool);
	}

	// Print the statistics for all of the various tree depths.
	for(intnative_t tree_Depth=minimum_Tree_Depth;
	  tree_Depth<=maximum_Tree_Depth; tree_Depth+=2)
		printf("%jd\t trees of depth %jd\t check: %jd\n",
		  (intmax_t)1<<(maximum_Tree_Depth-tree_Depth+minimum_Tree_Depth),
		  (intmax_t)tree_Depth,
		  (intmax_t)tree_Checksums[(tree_Depth-minimum_Tree_Depth)/2]);

	// Print the statistics for the long_Lived_Tree that was processed earlier
	// and then delete the memory_Pool that still is storing it up to this
	// point. Note that although the long_Lived_Tree variable isn't used here,
	// it still is in scope and valid to use until the call to
	// apr_pool_destroy(memory_Pool) is made.
	printf("long lived tree of depth %jd\t check: %jd\n",
	  (intmax_t)maximum_Tree_Depth,
	  (intmax_t)long_Lived_Tree_Checksum);
	apr_pool_destroy(memory_Pool);

	apr_terminate();
	return 0;
}
