interface XExpr {
}

final class ValXExpr implements XExpr {
  long value;
  ValXExpr(long i) {
    value = i;
  }
}

final class VarXExpr implements XExpr {
  long name;
  VarXExpr(long i) {
    name = i;
  }
}


final class AddXExpr implements XExpr {
  XExpr left;
  XExpr right;
  AddXExpr(XExpr l, XExpr r) {
    left = l;
    right = r;
  }
}

final class MulXExpr implements XExpr {
  XExpr left;
  XExpr right;
  MulXExpr(XExpr l, XExpr r) {
    left = l;
    right = r;
  }
}


public class cfold {
  static XExpr mk_expr( long n, long v ) {
    if (n == 0) {
      return (v==0 ? new VarXExpr(1) : new ValXExpr(v));
    }
    else {
      return new AddXExpr( mk_expr(n-1, v+1), mk_expr(n - 1, v == 0 ? 0 : v - 1));
    }
  }

  static XExpr append_add( XExpr e1, XExpr e2 ) {
    if (e1 instanceof AddXExpr a) {
      return new AddXExpr(a.left, append_add(a.right, e2));
    }
    else {
      return new AddXExpr(e1,e2);
    }
  }

  static XExpr tail_append_add( XExpr e1, XExpr e2 ) {
    AddXExpr hd  = null;
    AddXExpr acc = null;
    while(e1 instanceof AddXExpr x) {
      if (acc==null) {
        hd = acc = new AddXExpr(x.left,null);
      }
      else {
        AddXExpr y = new AddXExpr(x.left,null);
        acc.right = y;
        acc = y;
      }
      e1 = x.right;
    }
    if (acc==null) hd = acc = new AddXExpr(e1,e2);
              else acc.right = new AddXExpr(e1,e2);
    return hd;
  }

  static XExpr append_mul( XExpr e1, XExpr e2 ) {
    if (e1 instanceof MulXExpr a) {
      return new MulXExpr(a.left, append_mul(a.right, e2));
    }
    else {
      return new MulXExpr(e1,e2);
    }
  }

  static XExpr reassoc( XExpr e ) {
    if (e instanceof AddXExpr a) {
      return append_add( reassoc(a.left), reassoc(a.right) );
    }
    else if (e instanceof MulXExpr m) {
      return append_mul( reassoc(m.left), reassoc(m.right) );
    }
    else return e;
  }

  static XExpr const_folding( XExpr e ) {
    if (e instanceof AddXExpr x) {
      XExpr e1 = const_folding(x.left);
      XExpr e2 = const_folding(x.right);
      if (e1 instanceof ValXExpr a && e2 instanceof ValXExpr b) {
        return new ValXExpr(a.value + b.value );
      }
      else if (e1 instanceof ValXExpr a && e2 instanceof AddXExpr b && b.right instanceof ValXExpr br) {
        return new AddXExpr( new ValXExpr(a.value + br.value), b.left );
      }
      else if (e1 instanceof ValXExpr a && e2 instanceof AddXExpr b && b.left instanceof ValXExpr bl) {
        return new AddXExpr( new ValXExpr(a.value + bl.value), b.right );
      }
      else {
        return new AddXExpr(e1,e2);
      }
    }
    else if (e instanceof MulXExpr x) {
      XExpr e1 = const_folding(x.left);
      XExpr e2 = const_folding(x.right);
      if (e1 instanceof ValXExpr a && e2 instanceof ValXExpr b) {
        return new ValXExpr(a.value * b.value );
      }
      else if (e1 instanceof ValXExpr a && e2 instanceof MulXExpr b && b.right instanceof ValXExpr br) {
        return new MulXExpr( new ValXExpr(a.value * br.value), b.left );
      }
      else if (e1 instanceof ValXExpr a && e2 instanceof MulXExpr b && b.left instanceof ValXExpr bl) {
        return new MulXExpr( new ValXExpr(a.value * bl.value), b.right );
      }
      else {
        return new MulXExpr(e1,e2);
      }
    }
    else return e;
  }

  static long eval( XExpr e ) {
    if (e instanceof VarXExpr x) {
      return 0;
    }
    else if (e instanceof ValXExpr x) {
      return x.value;
    }
    else if (e instanceof AddXExpr x) {
      return eval(x.left) + eval(x.right);
    }
    else if (e instanceof MulXExpr x) {
      return eval(x.left) * eval(x.right);
    }
    else {
      return 0;
    }
  }

  public static void main(String args[])
  {
    XExpr e = mk_expr(20,1);
    long v1 = eval(e);
    long v2 = eval(const_folding(reassoc(e)));
    System.out.println( v1 + ", " + v2 );
  }
}
