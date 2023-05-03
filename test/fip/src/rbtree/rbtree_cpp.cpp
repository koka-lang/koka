// Using standard STL to test the red-black tree in C++
// In glibc++ this uses <https://github.com/gcc-mirror/gcc/tree/master/libstdc++-v3/src/c++98/tree.cc>
// With the LLVM libc++ this uses <https://github.com/llvm/llvm-project/blob/main/libcxx/include/__tree>
// In glibc this uses eventually: <https://sourceware.org/git/?p=glibc.git;a=blob;f=misc/tsearch.c>
// (Highly optimized in-place red-black tree using the low pointer bit to encode color information.)

#include <iostream>
#include <map>
#include <algorithm>
using std::for_each;

typedef int32_t nat;

struct nat_lt_fn {
    bool operator()(nat const & n1, nat const & n2) const { return n1 < n2; }
};

typedef std::map<nat, bool, nat_lt_fn> map;

map mk_map(unsigned n) {
    map m;
    while (n > 0) {
        --n;
        m.insert(std::make_pair(nat(n), n%10 == 0));
    }
    return m;
}

nat fold(map const & m) {
    nat r(0);
    for_each(m.begin(), m.end(), [&](std::pair<nat, bool> const & p) { if (p.second) r = r + nat(1); });
    return r;
}

/*
int main(int argc, char ** argv) {
    unsigned n = 4200000;
    if (argc == 2) {
      n = atoi(argv[1]);
    }
    map m = mk_map(n);
    std::cout << fold(m) << "\n";
    return 0;
}
*/

void test(int n) {
    int iter = 10000000 / (n <= 0 ? 1 : n);
    int32_t acc = 0;
    for(int i = 0; i < iter; i++) {
      map m = mk_map(n);
      acc += fold(m);
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