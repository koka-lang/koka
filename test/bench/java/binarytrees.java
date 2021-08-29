/**
 * The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * based on Jarkko Miettinen's Java program
 * contributed by Tristan Dupont
 * *reset*
 */

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class binarytrees {

    private static final int MIN_DEPTH = 4;
    private static final ExecutorService EXECUTOR_SERVICE = 
        Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

    public static void main(final String[] args) throws Exception {
        int n = 21;
        if (0 < args.length) {
            n = Integer.parseInt(args[0]);
        }

        final int maxDepth = n < (MIN_DEPTH + 2) ? MIN_DEPTH + 2 : n;
        final int stretchDepth = maxDepth + 1;

        System.out.println("stretch tree of depth " + stretchDepth + "\t check: " 
           + bottomUpTree( stretchDepth).itemCheck());

        final TreeNode longLivedTree = bottomUpTree(maxDepth);

        final String[] results = new String[(maxDepth - MIN_DEPTH) / 2 + 1];

        for (int d = MIN_DEPTH; d <= maxDepth; d += 2) {
            final int depth = d;
            EXECUTOR_SERVICE.execute(() -> {
                int check = 0;

                final int iterations = 1 << (maxDepth - depth + MIN_DEPTH);
                for (int i = 1; i <= iterations; ++i) {
                    final TreeNode treeNode1 = bottomUpTree(depth);
                    check += treeNode1.itemCheck();
                }
                results[(depth - MIN_DEPTH) / 2] = 
                   iterations + "\t trees of depth " + depth + "\t check: " + check;
            });
        }

        EXECUTOR_SERVICE.shutdown();
        EXECUTOR_SERVICE.awaitTermination(120L, TimeUnit.SECONDS);

        for (final String str : results) {
            System.out.println(str);
        }

        System.out.println("long lived tree of depth " + maxDepth + 
            "\t check: " + longLivedTree.itemCheck());
    }

    private static TreeNode bottomUpTree(final int depth) {
        if (0 < depth) {
            return new TreeNode(bottomUpTree(depth - 1), bottomUpTree(depth - 1));
        }
        return new TreeNode();
    }

    private static final class TreeNode {

        private final TreeNode left;
        private final TreeNode right;

        private TreeNode(final TreeNode left, final TreeNode right) {
            this.left = left;
            this.right = right;
        }

        private TreeNode() {
            this(null, null);
        }

        private int itemCheck() {
            // if necessary deallocate here
            if (null == left) {
                return 1;
            }
            return 1 + left.itemCheck() + right.itemCheck();
        }

    }

}
