// NQueens solution in C++
// Note: does not free memory as that is difficult to do
// since many subsolutions are shared
#include <iostream>

template <typename T>
class list {
public:
  T head;
  list<T>* tail;
  list(T hd, list<T>* tl) {
    head = hd;
    tail = tl;
  }
  ~list() {
    delete head;
    delete tail;
  }
};

template <typename T>
list<T>* Cons( T hd, list<T>* tl ) {
  return new list<T>(hd,tl);
}

template <typename T>
int len(list<T>* xs) {
  int n = 0;
  while(xs != NULL) {
    n++;
    xs = xs->tail;
  }
  return n;
}

bool safe( int queen, list<int>* xs ) {
  list<int>* cur = xs;
  int diag = 1;
  while(cur != NULL) {
    int q = cur->head;
    if (queen == q || queen == (q+diag) || queen == (q-diag)) {
      return false;
    }
    diag++;
    cur = cur->tail;
  }
  return true;
}

list<list<int>*>* append_safe( int k, list<int>* soln, list<list<int>*>* solns ) {
  list<list<int>*>* acc = solns;
  int n = k;
  while(n > 0) {
    if (safe(n,soln)) {
      acc = Cons(Cons(n,soln),acc);
    }
    n--;
  }
  return acc;
}

list<list<int>*>* extend( int n, list<list<int>*>* solns ) {
  list<list<int>*>* acc = NULL;
  list<list<int>*>* cur = solns;
  while(cur != NULL) {
    list<int>* soln = cur->head;
    acc = append_safe(n,soln,acc);
    cur = cur->tail;
  }
  return acc;
}

list<list<int>*>* find_solutions( int n ) {
  int k = 0;
  list<list<int>*>* acc = Cons<list<int>*>(NULL,NULL);
  while(k < n) {
    acc = extend(n,acc);
    k++;
  }
  return acc;
}

int nqueens(int n) {
  return len(find_solutions(n));
}

int main(int argc, char ** argv) {
    int n = 13;
    if (argc == 2) {
      n = atoi(argv[1]);
    }
    std::cout << nqueens(n) << "\n";
    return 0;
}
