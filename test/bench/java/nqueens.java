class List<T> {
  T head;
  List<T> tail;

  List(T h, List<T> t) {
    head = h;
    tail = t;
  }

  static <T> int len( List<T> xs ) {
    int n = 0;
    while(xs != null) {
      n++;
      xs = xs.tail;
    }
    return n;
  }

  static<T> List<T> Cons( T h, List<T> t ) {
    return new List<T>(h,t);
  }

}

public class nqueens {
  static boolean safe( int queen, List<Integer> xs ) {
    int diag = 1;
    while(xs != null) {
      int q = xs.head;
      if (queen == q || queen == (q + diag) || queen == (q - diag)) {
        return false;
      }
      diag++;
      xs = xs.tail;
    }
    return true;
  }

  static List<List<Integer>> appendSafe( int k, List<Integer> soln, List<List<Integer>> solns ) {
    List<List<Integer>> acc = solns;
    while(k > 0) {
      if (safe(k,soln)) {
        acc = List.Cons(List.Cons(k,soln),acc);
      }
      k--;
    }
    return acc;
  }

  static List<List<Integer>> extend( int n, List<List<Integer>> solns ) {
    List<List<Integer>> acc = null;
    List<List<Integer>> cur = solns;
    while(cur != null) {
      acc = appendSafe(n, cur.head, acc);
      cur = cur.tail;
    }
    return acc;
  }

  static List<List<Integer>> findSolutions( int n ) {
    int k = 0;
    List<List<Integer>> acc = List.Cons(null,null);
    while(k < n) {
      acc = extend(n,acc);
      k++;
    }
    return acc;
  }


  static int nqueens(int n) {
    return List.len(findSolutions(n));
  }
  public static void main(String args[])
  {
      System.out.println( nqueens(13) );
  }
}
