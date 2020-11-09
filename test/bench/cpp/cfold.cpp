#include <math.h>
#include <string.h>
#include <iostream>

enum Kind {
  Val,
  Var,
  Add,
  Mul,
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
  long name;
  VarExpr( long n ) : Expr(Var) {
    this->name = n;
  }
};

class AddExpr : public Expr {
public:
  const Expr* left;
  const Expr* right;
  AddExpr( const Expr* e1, const Expr* e2 ) : Expr(Add) {
    this->left = e1;
    this->right = e2;
  }
};

class MulExpr : public Expr {
public:
  const Expr* left;
  const Expr* right;
  MulExpr( const Expr* e1, const Expr* e2 ) : Expr(Mul) {
    this->left = e1;
    this->right = e2;
  }
};


static const Expr* mk_expr( long n, long v ) {
  if (n==0) {
    if (v==0) return new VarExpr(1);
         else return new ValExpr(v);
  }
  else {
    return new AddExpr(mk_expr(n-1,v+1),mk_expr(n-1,v == 0 ? 0 : v - 1));
  }
}

static const Expr* append_add( const Expr* e1, const Expr* e2 ) {
  if (e1->kind == Add) {
    const AddExpr* x = (AddExpr*)e1;
    return new AddExpr( x->left, append_add(x->right, e2));
  }
  else {
    return new AddExpr(e1,e2);
  }
}

static const Expr* append_mul( const Expr* e1, const Expr* e2 ) {
  if (e1->kind == Mul) {
    const MulExpr* x = (MulExpr*)e1;
    return new MulExpr( x->left, append_mul(x->right, e2));
  }
  else {
    return new MulExpr(e1,e2);
  }
}

static const Expr* reassoc( const Expr* e ) {
  if (e->kind == Add) {
    const AddExpr* x = (AddExpr*)e;
    return append_add( reassoc(x->left), reassoc(x->right) );
  }
  else if (e->kind == Mul) {
    const MulExpr* x = (MulExpr*)e;
    return append_mul( reassoc(x->left), reassoc(x->right) );
  }
  else return e;
}

static const Expr* const_folding( const Expr* e ) {
  if (e->kind == Add) {
    const Expr* e1 = ((AddExpr*)e)->left;
    const Expr* e2 = ((AddExpr*)e)->right;
    if (e1->kind == Val && e2->kind==Val) {
      return new ValExpr( ((ValExpr*)e1)->value + ((ValExpr*)e2)->value );
    }
    else if (e1->kind == Val && e2->kind==Add && ((AddExpr*)e2)->right->kind == Val) {
      AddExpr* b = (AddExpr*)e2;
      ValExpr* v = (ValExpr*)(b->right);
      return new AddExpr( new ValExpr(((ValExpr*)e1)->value + v->value ), b->left );
    }
    else if (e1->kind == Val && e2->kind==Add && ((AddExpr*)e2)->left->kind == Val) {
      AddExpr* b = (AddExpr*)e2;
      ValExpr* v = (ValExpr*)(b->left);
      return new AddExpr( new ValExpr(((ValExpr*)e1)->value + v->value ), b->right );
    }
    else {
      return new AddExpr(e1,e2);
    }
  }
  else if (e->kind == Mul) {
    const Expr* e1 = ((MulExpr*)e)->left;
    const Expr* e2 = ((MulExpr*)e)->right;
    if (e1->kind == Val && e2->kind==Val) {
      return new ValExpr( ((ValExpr*)e1)->value * ((ValExpr*)e2)->value );
    }
    else if (e1->kind == Val && e2->kind==Mul && ((MulExpr*)e2)->right->kind == Val) {
      MulExpr* b = (MulExpr*)e2;
      ValExpr* v = (ValExpr*)(b->right);
      return new MulExpr( new ValExpr(((ValExpr*)e1)->value * v->value ), b->left );
    }
    else if (e1->kind == Val && e2->kind==Mul && ((MulExpr*)e2)->left->kind == Val) {
      MulExpr* b = (MulExpr*)e2;
      ValExpr* v = (ValExpr*)(b->left);
      return new MulExpr( new ValExpr(((ValExpr*)e1)->value * v->value ), b->right );
    }
    else {
      return new MulExpr(e1,e2);
    }
  }
  else return e;
}

static long eval( const Expr* e ) {
  if (e->kind == Var) {
    return 0;
  }
  else if (e->kind == Val) {
    return ((ValExpr*)e)->value;
  }
  else if (e->kind == Add) {
    return eval(((AddExpr*)e)->left) + eval(((AddExpr*)e)->right);
  }
  else if (e->kind == Mul) {
    return eval(((MulExpr*)e)->left) * eval(((MulExpr*)e)->right);
  }
  else {
    return 0;
  }
}


int main(int argc, char ** argv) {
  const Expr* e = mk_expr(20,1);
  long v1 = eval(e);
  long v2 = eval(const_folding(reassoc(e)));
  std::cout << v1 << ", " << v2 << "\n";
  return 0;
}
