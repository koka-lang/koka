// As a mini benchmark, this is an in-place updating version of `rbtree.kk` in C++
// using the standard STL `std::map` library which uses internally a highly optimized
// red-black trees as well.
//
// Note: This is a bit unfair to `rbtree.kk` as that uses polymorphic trees with infinite
// precision integers and generic folding; and is purely functional and thus also supports "persistent"
// shared trees as well. A more direct comparison can be found in the benchmarks (`test/bench/cpp` and `test/bench/koka`).
//
// The `std::map` uses eventually <https://github.com/gcc-mirror/gcc/tree/master/libstdc++-v3/src/c++98/tree.cc>
// with glib++, and <https://github.com/llvm/llvm-project/blob/main/libcxx/include/__tree> with the LLVM libc++.
// (in older glibc it uses: <https://sourceware.org/git/?p=glibc.git;a=blob;f=misc/tsearch.c>,
//  a highly optimized in-place red-black tree using the low pointer bit to encode color information.)
//
// Compile as: > g++ --std=c++17 -O3 -o cpp-rbtree  samples/basic/rbtree.cpp
//
// as of 2021-09-08, koka v2.3.1, clang++ 10.0.0, g++ 9.3.0, ubuntu 20.04, 32Gb AMD5950x:
//
// $ tm .koka/v2.3.1/gcc-drelease/samples_basic_rbtree
// 420000
// elapsed: 0.52s, user: 0.49s, sys: 0.01s, rss: 203180kb
// $ tm bench/gcc_samples_rbtree
// 420000
// elapsed: 0.52s, user: 0.48s, sys: 0.02s, rss: 200344kb
// tm bench/clang_samples_rbtree
// 420000
// elapsed: 0.65s, user: 0.63s, sys: 0.02s, rss: 200228kb

#include <iostream>
#include <map>
#include <algorithm>
using std::for_each;

// Specialize a tree for `int` keys with `bool` elements
struct int_lt_fn {
    bool operator()(int const & n1, int const & n2) const { return n1 < n2; }
};

typedef std::map<int, bool, int_lt_fn> map;

// Create a tree of `n` elements
map make_tree(int n) {
    map t;
    while (n > 0) {
        --n;
        t.insert(std::make_pair(n, n%10 == 0));
    }
    return t;
}

// Count all `true` elements in the tree (specialized fold)
int fold(map const & t) {
    int r = 0;
    for_each(t.begin(), t.end(), [&](std::pair<int, bool> const & p) { if (p.second) r = r + 1; });
    return r;
}

// The benchmark as in `rbtree`
void bench(int n) {
    map t = make_tree(n);
    std::cout << fold(t) << std::endl;
}

int main(int argc, char ** argv) {
    int n = 4200000;
    if (argc == 2) {
      n = atoi(argv[1]);
    }
    bench(n);
    return 0;
}
