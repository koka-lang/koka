#include <math.h>
#include <string.h>
#include <iostream>

enum Kind {
  Val,
  Var,
  Add,
  Mul,
  Pow,
  Ln
};

class Expr {
public:
  Kind kind;
  Expr(Kind k) {
    this->kind = k;
  }
};

class ValExpr : public Expr {
public:
  long value;
  ValExpr(long i) : Expr(Val) {
    this->value = i;
  }
};

class VarExpr : public Expr {
public:
  const char* name;
  VarExpr( const char* n ) : Expr(Var) {
    this->name = n;
  }
};

class UnaryExpr : public Expr {
public:
  const Expr* expr;
  UnaryExpr( Kind k, const Expr* e ) : Expr(k) {
    this->expr = e;
  }
};

class BinExpr : public Expr {
public:
  const Expr* left;
  const Expr* right;
  BinExpr( Kind k, const Expr* e1, const Expr* e2 ) : Expr(k) {
    this->left = e1;
    this->right = e2;
  }

};

static long pown(long x, long n) {
  if (n==0) return 1;
  else if (n == 1) return x;
  else {
    long y = pown(x, n/2);
    return (y * y * (n%2 == 0 ? 1 : x));
  }
}

static const Expr* add( const Expr* x, const Expr* y ) {
  if (x->kind == Val && y->kind == Val) {
    return new ValExpr(((ValExpr*)x)->value + ((ValExpr*)y)->value);
  }
  else if (x->kind==Val && ((ValExpr*)x)->value==0) {
    return y;
  }
  else if (y->kind==Val && ((ValExpr*)y)->value==0) {
    return x;
  }
  else if (y->kind==Val) {
    return add(y,x);
  }
  else if (x->kind==Val && y->kind==Add && ((BinExpr*)y)->left->kind==Val) {
    long lval = ((ValExpr*)((BinExpr*)y)->left)->value;
    return add(new ValExpr(((ValExpr*)x)->value + lval), ((BinExpr*)y)->right);
  }
  else if (y->kind==Add && ((BinExpr*)y)->left->kind==Val) {
    return add(((BinExpr*)y)->left,add(x,((BinExpr*)y)->right));
  }
  else if (x->kind==Add) {
    return add(((BinExpr*)x)->left,add(((BinExpr*)x)->right,y));
  }
  else {
    return new BinExpr(Add,x,y);
  }
}

static const Expr* mul( const Expr* x, const Expr* y ) {
  if (x->kind == Val && y->kind == Val) {
    return new ValExpr(((ValExpr*)x)->value * ((ValExpr*)y)->value);
  }
  else if (x->kind==Val && ((ValExpr*)x)->value==0) {
    return x;
  }
  else if (y->kind==Val && ((ValExpr*)y)->value==0) {
    return y;
  }
  else if (x->kind==Val && ((ValExpr*)x)->value==1) {
    return y;
  }
  else if (y->kind==Val && ((ValExpr*)y)->value==1) {
    return x;
  }
  else if (y->kind==Val) {
    return mul(y,x);
  }
  else if (x->kind==Val && y->kind==Mul && ((BinExpr*)y)->left->kind==Val) {
    long lval = ((ValExpr*)((BinExpr*)y)->left)->value;
    return mul(new ValExpr(((ValExpr*)x)->value * lval), ((BinExpr*)y)->right);
  }
  else if (y->kind==Mul && ((BinExpr*)y)->left->kind==Val) {
    return mul(((BinExpr*)y)->left,mul(x,((BinExpr*)y)->right));
  }
  else if (x->kind==Mul) {
    return mul(((BinExpr*)x)->left,mul(((BinExpr*)x)->right,y));
  }
  else {
    return new BinExpr(Mul,x,y);
  }
}

static const Expr* powr( const Expr* x, const Expr* y) {
  if (x->kind == Val && y->kind == Val) {
    return new ValExpr( pown(((ValExpr*)x)->value,((ValExpr*)y)->value));
  }
  else if (y->kind==Val && ((ValExpr*)y)->value == 0) {
    return new ValExpr(1);
  }
  else if (y->kind==Val && ((ValExpr*)y)->value == 1) {
    return x;
  }
  else if (x->kind==Val && ((ValExpr*)x)->value == 0) {
    return new ValExpr(0);
  }
  else {
    return new BinExpr(Pow,x,y);
  }
}

static const Expr* ln(const Expr* n) {
  if (n->kind == Val && ((ValExpr*)n)->value == 1) {
    return new ValExpr(0);
  }
  else {
    return new UnaryExpr(Ln,n);
  }
}

static const Expr* d( const char* x, const Expr* e) {
  if (e->kind == Val) {
    return new ValExpr(0);
  }
  else if (e->kind==Var) {
    return new ValExpr( strcmp(((VarExpr*)e)->name,x)==0 ? 1 : 0);
  }
  else if (e->kind==Add) {
    const Expr* f = ((BinExpr*)e)->left;
    const Expr* g = ((BinExpr*)e)->right;
    return add(d(x,f),d(x,g));
  }
  else if (e->kind==Mul) {
    const Expr* f = ((BinExpr*)e)->left;
    const Expr* g = ((BinExpr*)e)->right;
    return add(mul(f,d(x,g)),mul(g,d(x,f)));
  }
  else if (e->kind==Pow) {
    const Expr* f = ((BinExpr*)e)->left;
    const Expr* g = ((BinExpr*)e)->right;
    return  mul(powr(f,g),add(mul(mul(g,d(x,f)),powr(f,new ValExpr(-1))),mul(ln(f),d(x,g))));
  }
  else { // if (e->kind==Ln) {
    const Expr* f = ((UnaryExpr*)e)->expr;
    return mul(d(x,f),powr(f,new ValExpr(-1)));
  }
}

static long count( const Expr* e) {
  if (e->kind == Val) {
    return 1;
  }
  else if (e->kind==Var) {
    return 1;
  }
  else if (e->kind==Add) {
    const Expr* f = ((BinExpr*)e)->left;
    const Expr* g = ((BinExpr*)e)->right;
    return count(f) + count(g);
  }
  else if (e->kind==Mul) {
    const Expr* f = ((BinExpr*)e)->left;
    const Expr* g = ((BinExpr*)e)->right;
    return count(f) + count(g);
  }
  else if (e->kind==Pow) {
    const Expr* f = ((BinExpr*)e)->left;
    const Expr* g = ((BinExpr*)e)->right;
    return count(f) + count(g);
  }
  else { // if (e->kind==Ln) {
    const Expr* f = ((UnaryExpr*)e)->expr;
    return count(f);
  }
}

static const Expr* deriv(long i, const Expr* e) {
  const Expr* f = d("x",e);
  std::cout << (i+1) << " count: " << count(f) << "\n";
  return f;
}

static const Expr* nest( long s, const Expr* e) {
  long n = s;
  while(n > 0) {
    e = deriv(s - n, e);
    n--;
  }
  return e;
}


int main(int argc, char ** argv) {
  const Expr* x = new VarExpr("x");
  const Expr* e = powr(x,x);
  nest(10,e);
  std::cout << "done\n";
  return 0;
}
