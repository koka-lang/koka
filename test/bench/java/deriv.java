interface Expr {
}

final class ValExpr implements Expr {
  long value;
  ValExpr(long i) {
    value = i;
  }
}

final class VarExpr implements Expr {
  String name;
  VarExpr(String s) {
    name = s;
  }
}

final class LnExpr implements Expr {
  Expr expr;
  LnExpr(Expr e) {
    expr = e;
  }
}

final class AddExpr implements Expr {
  Expr left;
  Expr right;
  AddExpr(Expr l, Expr r) {
    left = l;
    right = r;
  }
}

final class MulExpr implements Expr {
  Expr left;
  Expr right;
  MulExpr(Expr l, Expr r) {
    left = l;
    right = r;
  }
}

final class PowExpr implements Expr {
  Expr left;
  Expr right;
  PowExpr(Expr l, Expr r) {
    left = l;
    right = r;
  }
}



public class deriv {
  static long pown(long x, long n) {
    if (n==0) return 1;
    else if (n == 1) return x;
    else {
      long y = pown(x, n/2);
      return (y * y * (n%2 == 0 ? 1 : x));
    }
  }

  static Expr add( Expr x, Expr y ) {
    if (x instanceof ValExpr a && y instanceof ValExpr b) {
      return new ValExpr( a.value + b.value );
    }
    else if (x instanceof ValExpr a && a.value == 0) {
      return y;
    }
    else if (y instanceof ValExpr b && b.value == 0) {
      return x;
    }
    else if (y instanceof ValExpr b) {
      return add(y,x);
    }
    else if (x instanceof ValExpr a && y instanceof AddExpr b && b.left instanceof ValExpr bl) {
      return add(new ValExpr(a.value + bl.value), b.right);
    }
    else if (y instanceof AddExpr b && b.left instanceof ValExpr) {
      return add(b.left, add(x,b.right));
    }
    else if (x instanceof AddExpr a) {
      return add(a.left, add(a.right,y));
    }
    else {
      return new AddExpr(x,y);
    }
  }

  static Expr mul( Expr x, Expr y ) {
    if (x instanceof ValExpr a && y instanceof ValExpr b) {
      return new ValExpr( a.value * b.value );
    }
    else if (x instanceof ValExpr a && a.value == 0) {
      return x;
    }
    else if (y instanceof ValExpr b && b.value == 0) {
      return y;
    }
    else if (x instanceof ValExpr a && a.value == 1) {
      return y;
    }
    else if (y instanceof ValExpr b && b.value == 1) {
      return x;
    }
    else if (y instanceof ValExpr b) {
      return mul(y,x);
    }
    else if (x instanceof ValExpr a && y instanceof MulExpr b && b.left instanceof ValExpr bl) {
      return mul(new ValExpr(a.value * bl.value), b.right);
    }
    else if (y instanceof MulExpr b && b.left instanceof MulExpr) {
      return mul(b.left, mul(x,b.right));
    }
    else if (x instanceof MulExpr a) {
      return mul(a.left, mul(a.right,y));
    }
    else {
      return new MulExpr(x,y);
    }
  }

  static Expr powr( Expr x, Expr y ) {
    if (x instanceof ValExpr a && y instanceof ValExpr b) {
      return new ValExpr(pown(a.value,b.value));
    }
    else if (y instanceof ValExpr b && b.value == 0) {
      return new ValExpr(1);
    }
    else if (y instanceof ValExpr b && b.value == 1) {
      return x;
    }
    else if (x instanceof ValExpr a && a.value == 0) {
      return new ValExpr(0);
    }
    else {
      return new PowExpr(x,y);
    }
  }

  static Expr ln( Expr x ) {
    if (x instanceof ValExpr a && a.value == 1) {
      return new ValExpr(0);
    }
    else {
      return new LnExpr(x);
    }
  }

  static Expr d( String x, Expr e  ) {
    if (e instanceof ValExpr) {
      return new ValExpr(0);
    }
    else if (e instanceof VarExpr a) {
      return new ValExpr(a.name == x ? 1 : 0);
    }
    else if (e instanceof AddExpr a) {
      Expr f = a.left;
      Expr g = a.right;
      return add(d(x,f),d(x,g));
    }
    else if (e instanceof MulExpr a) {
      Expr f = a.left;
      Expr g = a.right;
      return add(mul(f,d(x,g)),mul(g,d(x,f)));
    }
    else if (e instanceof PowExpr a) {
      Expr f = a.left;
      Expr g = a.right;
      return mul(powr(f,g),add(mul(mul(g,d(x,f)),powr(f,new ValExpr(-1))),mul(ln(f),d(x,g))));
    }
    else if (e instanceof LnExpr a) {
      Expr f = a.expr;
      return mul(d(x,f),powr(f,new ValExpr(-1)));
    }
    else {
      return e;
    }
  }

  static long count( Expr e  ) {
    if (e instanceof ValExpr) {
      return 1;
    }
    else if (e instanceof VarExpr) {
      return 1;
    }
    else if (e instanceof AddExpr a) {
      Expr f = a.left;
      Expr g = a.right;
      return count(f) + count(g);
    }
    else if (e instanceof MulExpr a) {
      Expr f = a.left;
      Expr g = a.right;
      return count(f) + count(g);
    }
    else if (e instanceof PowExpr a) {
      Expr f = a.left;
      Expr g = a.right;
      return count(f) + count(g);
    }
    else if (e instanceof LnExpr a) {
      Expr f = a.expr;
      return count(f);
    }
    else {
      return 0;
    }
  }

  static Expr deriv( long i, Expr e) {
    Expr f = d("x",e);
    System.out.println( (i+1) + " count: " + count(f) );
    return f;
  }

  static Expr nest( long s, Expr e) {
    long n = s;
    while(n > 0) {
      e = deriv(s - n, e);
      n--;
    }
    return e;
  }

  public static void main(String args[])
  {
    Expr x = new VarExpr("x");
    Expr e = powr(x,x);
    nest(10,e);
    System.out.println( "done" );
  }
}
