// Try persisting std:map through copying; is too slow... 
// We should try to make a persistent RB tree in C++ but this is not trivial to do...
#include <iostream>
#include <map>
#include <list>
#include <algorithm>
#include <memory>
// #include "util/nat.h"
// #include "util/list.h"
// using namespace lean;
using std::for_each;
using std::list;

typedef int nat;

struct nat_lt_fn {
    bool operator()(nat const & n1, nat const & n2) const { return n1 < n2; }
};

typedef std::map<nat, bool, nat_lt_fn> map_t;
typedef std::shared_ptr<map_t> map;

list<map>& cons(map m, list<map>& stack) {
  stack.push_front(m);
  return stack;
}

map head(list<map>& stack) {
  return stack.front();
}

list<map> mk_map(unsigned n, unsigned freq) {
    list<map> stack;
    auto m = std::make_shared<map_t>();
    while (n > 0) {
        --n;
        m->insert(std::make_pair(nat(n), n%10 == 0));
        if (n % freq == 0) {
          stack = cons(std::make_shared<map_t>(*m) /* copy constructor */, stack);
        }
    }
    stack = cons(m, stack);
    return stack;
}

nat fold(map const & m) {
    nat r(0);
    for_each(m->begin(), m->end(), [&](std::pair<nat, bool> const & p) { if (p.second) r = r + nat(1); });
    return r;
}

int main(int argc, char ** argv) {
    unsigned n = 4200; // 4200000;
    unsigned freq = 5;
    if (argc == 3) {
      n = atoi(argv[1]);
      freq = atoi(argv[2]);
    }
    list<map> m = mk_map(n, freq);
    std::cout << fold(head(m)) << "\n";
    return 1;  // signal that this test is not working
}
