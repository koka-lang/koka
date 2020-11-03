
enum Color {
  Red,
  Black
}


interface FoldFun {
  int Apply(int k, boolean v, int acc);
}

class Tree {
  Color color;
  Tree left;
  int  key;
  boolean val;
  Tree right;

  Tree( Color c, Tree l, int k, boolean v, Tree r)  {
    color = c;
    left = l;
    key = k;
    val = v;
    right = r;
  }

  static Tree Node( Color c, Tree l, int k, boolean v, Tree r)  {
    return new Tree(c,l,k,v,r);
  }

  static boolean isRed( Tree t ) {
    return (t != null && t.color == Color.Red);
  }

  static Tree balanceRight( int kv, boolean vv, Tree t, Tree n ) {
    if (n == null) {
      return null;
    }
    else if (n.left != null && n.left.color == Color.Red) {
      //case let .Node(_, .Node(.Red, l, kx, vx, r1), ky, vy, r2):
      //  return .Node(.Red, .Node(.Black, l, kx, vx, r1), ky, vy, .Node(.Black, r2, kv, vv, t))
      Tree l = n.left;
      return Node( Color.Red, Node( Color.Black, l.left, l.key, l.val, l.right), n.key, n.val, Node(Color.Black, n.right, kv, vv, t));
    }
    else if (n.right != null && n.right.color == Color.Red) {
      //case let .Node(_, l1, ky, vy, .Node(.Red, l2, kx, vx, r)):
      //  return .Node(.Red, .Node(.Black, l1, ky, vy, l2), kx, vx, .Node(.Black, r, kv, vv, t))
      Tree r = n.right;
      return Node( Color.Red, Node( Color.Black, n.left, n.key, n.val, r.left), r.key, r.val, Node(Color.Black, r.right, kv, vv, t));
    }
    else {
      //case let .Node(_, l, ky, vy, r):
      //  return .Node(.Black, .Node(.Red, l, ky, vy, r), kv, vv, t)
      return Node(Color.Black, Node(Color.Red, n.left, n.key, n.val, n.right), kv, vv, t);
    }
  }

  static Tree balanceLeft( Tree t, int kv, boolean vv, Tree n ) {
    if (n == null) {
      return null;
    }
    else if (n.left != null && n.left.color == Color.Red) {
      //case let .Node(_, .Node(.Red, l, kx1, vx1, r1), ky, vy, r2):
      //  return .Node(.Red, .Node(.Black, t, kv, vv, l), kx1, vx1, .Node(.Black, r1, ky, vy, r2))
      Tree l = n.left;
      return Node( Color.Red, Node( Color.Black, t, kv, vv, l.left), l.key, l.val, Node(Color.Black, l.right, n.key, n.val, n.right));
    }
    else if (n.right != null && n.right.color == Color.Red) {
      //case let .Node(_, l1, ky, vy, .Node(.Red, l2, kx2, vx2, r2)):
      //  return .Node(.Red, .Node(.Black, t, kv, vv, l1), ky, vy, .Node(.Black, l2, kx2, vx2, r2))
      Tree r = n.right;
      return Node( Color.Red, Node( Color.Black, t, kv, vv, n.left), n.key, n.val, Node(Color.Black, r.left, r.key, r.val, r.right));
    }
    else {
      //case let .Node (_, l, ky, vy, r):
      //  return .Node(.Black, t, kv, vv, .Node(.Red, l, ky, vy, r))
      return Node(Color.Black, t, kv, vv, Node(Color.Red, n.left, n.key, n.val, n.right));
    }
  }

  static Tree ins(Tree t, int kx, boolean vx ) {
    if (t==null) {
      return Node(Color.Red, null, kx, vx, null);
    }
    else if (t.color == Color.Red) {
      //case let .Node(.Red, a, ky, vy, b):
      if (kx < t.key) {
        return Node(Color.Red, ins(t.left, kx, vx), t.key, t.val, t.right);
      } else if (t.key == kx) {
        return Node(Color.Red, t.left, kx, vx, t.right);
      } else {
        return Node(Color.Red, t.left, t.key, t.val, ins(t.right, kx, vx));
      }
    }
    else { // t.color == Black
      if (kx < t.key) {
        if (isRed(t.left)) {
          return balanceRight(t.key, t.val, t.right, ins(t.left, kx, vx));
        } else {
          return Node(Color.Black, ins(t.left, kx, vx), t.key, t.val, t.right);
        }
      } else if (kx == t.key) {
        return Node(Color.Black, t.left, kx, vx, t.right);
      } else {
        if (isRed(t.right)) {
          return balanceLeft(t.left, t.key, t.val, ins(t.right, kx, vx));
        } else {
          return Node(Color.Black, t.left, t.key, t.val, ins(t.right, kx, vx));
        }
      }
    }
  }

  static Tree setBlack( Tree t ) {
    if (t == null) return t;
    return Node(Color.Black, t.left, t.key, t.val, t.right);
  }

  static Tree insert (Tree t, int k, boolean v) {
    if (isRed(t)) {
      return setBlack(ins(t, k, v));
    } else {
      return ins(t, k, v);
    }
  }

  static int Fold( FoldFun f, Tree t, int acc ) {
    while(t != null) {
      acc = Fold(f,t.left,acc);
      acc = f.Apply(t.key,t.val,acc);
      t = t.right;
    }
    return acc;
  }

}


public class rbtree
{
  static Tree mkMap( int n ) {
    Tree t = null;
    while(n > 0) {
      n--;
      t = Tree.insert(t, n, (n%10)==0);
    }
    return t;
  }

  static int Test(int n ) {
    Tree t = mkMap(n);
    return Tree.Fold( (k,v,acc) -> { return (v ? acc + 1 : acc); }, t, 0);
  }

  public static void main(String args[])
  {
      System.out.println( Test(4200000) );
  }
}
