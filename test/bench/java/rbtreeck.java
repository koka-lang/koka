
enum XColor {
  Red,
  Black
}

class TreeList {
  XTree head;
  TreeList tail;

  TreeList( XTree hd, TreeList tl ) {
    head = hd;
    tail = tl;
  }

  static TreeList Cons( XTree hd, TreeList tl ) {
    return new TreeList(hd,tl);
  }

  static TreeList Nil() {
    return null;
  }

  static int Len(TreeList xs) {
    int n = 0;
    while(xs != null) {
      if (xs.head != null) n++;
      xs = xs.tail;
    }
    return n;
  }
}


class XTree {
  XColor color;
  XTree left;
  int  key;
  boolean val;
  XTree right;

  XTree( XColor c, XTree l, int k, boolean v, XTree r)  {
    color = c;
    left = l;
    key = k;
    val = v;
    right = r;
  }

  static XTree Node( XColor c, XTree l, int k, boolean v, XTree r)  {
    return new XTree(c,l,k,v,r);
  }

  static boolean isRed( XTree t ) {
    return (t != null && t.color == XColor.Red);
  }

  static XTree balanceRight( int kv, boolean vv, XTree t, XTree n ) {
    if (n == null) {
      return null;
    }
    else if (n.left != null && n.left.color == XColor.Red) {
      //case let .Node(_, .Node(.Red, l, kx, vx, r1), ky, vy, r2):
      //  return .Node(.Red, .Node(.Black, l, kx, vx, r1), ky, vy, .Node(.Black, r2, kv, vv, t))
      XTree l = n.left;
      return Node( XColor.Red, Node( XColor.Black, l.left, l.key, l.val, l.right), n.key, n.val, Node(XColor.Black, n.right, kv, vv, t));
    }
    else if (n.right != null && n.right.color == XColor.Red) {
      //case let .Node(_, l1, ky, vy, .Node(.Red, l2, kx, vx, r)):
      //  return .Node(.Red, .Node(.Black, l1, ky, vy, l2), kx, vx, .Node(.Black, r, kv, vv, t))
      XTree r = n.right;
      return Node( XColor.Red, Node( XColor.Black, n.left, n.key, n.val, r.left), r.key, r.val, Node(XColor.Black, r.right, kv, vv, t));
    }
    else {
      //case let .Node(_, l, ky, vy, r):
      //  return .Node(.Black, .Node(.Red, l, ky, vy, r), kv, vv, t)
      return Node(XColor.Black, Node(XColor.Red, n.left, n.key, n.val, n.right), kv, vv, t);
    }
  }

  static XTree balanceLeft( XTree t, int kv, boolean vv, XTree n ) {
    if (n == null) {
      return null;
    }
    else if (n.left != null && n.left.color == XColor.Red) {
      //case let .Node(_, .Node(.Red, l, kx1, vx1, r1), ky, vy, r2):
      //  return .Node(.Red, .Node(.Black, t, kv, vv, l), kx1, vx1, .Node(.Black, r1, ky, vy, r2))
      XTree l = n.left;
      return Node( XColor.Red, Node( XColor.Black, t, kv, vv, l.left), l.key, l.val, Node(XColor.Black, l.right, n.key, n.val, n.right));
    }
    else if (n.right != null && n.right.color == XColor.Red) {
      //case let .Node(_, l1, ky, vy, .Node(.Red, l2, kx2, vx2, r2)):
      //  return .Node(.Red, .Node(.Black, t, kv, vv, l1), ky, vy, .Node(.Black, l2, kx2, vx2, r2))
      XTree r = n.right;
      return Node( XColor.Red, Node( XColor.Black, t, kv, vv, n.left), n.key, n.val, Node(XColor.Black, r.left, r.key, r.val, r.right));
    }
    else {
      //case let .Node (_, l, ky, vy, r):
      //  return .Node(.Black, t, kv, vv, .Node(.Red, l, ky, vy, r))
      return Node(XColor.Black, t, kv, vv, Node(XColor.Red, n.left, n.key, n.val, n.right));
    }
  }

  static XTree ins(XTree t, int kx, boolean vx ) {
    if (t==null) {
      return Node(XColor.Red, null, kx, vx, null);
    }
    else if (t.color == XColor.Red) {
      //case let .Node(.Red, a, ky, vy, b):
      if (kx < t.key) {
        return Node(XColor.Red, ins(t.left, kx, vx), t.key, t.val, t.right);
      } else if (t.key == kx) {
        return Node(XColor.Red, t.left, kx, vx, t.right);
      } else {
        return Node(XColor.Red, t.left, t.key, t.val, ins(t.right, kx, vx));
      }
    }
    else { // t.color == Black
      if (kx < t.key) {
        if (isRed(t.left)) {
          return balanceRight(t.key, t.val, t.right, ins(t.left, kx, vx));
        } else {
          return Node(XColor.Black, ins(t.left, kx, vx), t.key, t.val, t.right);
        }
      } else if (kx == t.key) {
        return Node(XColor.Black, t.left, kx, vx, t.right);
      } else {
        if (isRed(t.right)) {
          return balanceLeft(t.left, t.key, t.val, ins(t.right, kx, vx));
        } else {
          return Node(XColor.Black, t.left, t.key, t.val, ins(t.right, kx, vx));
        }
      }
    }
  }

  static XTree setBlack( XTree t ) {
    if (t == null) return t;
    return Node(XColor.Black, t.left, t.key, t.val, t.right);
  }

  static XTree insert (XTree t, int k, boolean v) {
    if (isRed(t)) {
      return setBlack(ins(t, k, v));
    } else {
      return ins(t, k, v);
    }
  }

  interface XFoldFun {
    int Apply(int k, boolean v, int acc);
  }

  static int Fold( XFoldFun f, XTree t, int acc ) {
    while(t != null) {
      acc = Fold(f,t.left,acc);
      acc = f.Apply(t.key,t.val,acc);
      t = t.right;
    }
    return acc;
  }

}


public class rbtreeck
{
  static TreeList mkMap( int n, int freq ) {
    XTree t = null;
    TreeList xs = null;
    while(n > 0) {
      n--;
      t = XTree.insert(t, n, (n%10)==0);
      if (n%freq == 0) {
        xs = TreeList.Cons(t,xs);
      }
    }
    return TreeList.Cons(t,xs);
  }

  static int Test(int n, int freq ) {
    TreeList xs = mkMap(n,freq);
    int res = XTree.Fold( (k,v,acc) -> { return (v ? acc + 1 : acc); }, xs.head, 0);
    System.out.println( TreeList.Len(xs) );
    return res;
  }

  public static void main(String args[])
  {
    System.out.println( Test(4200000, 5) );
  }
}
