/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
using System.Text;

public static class Primitive
{
  //---------------------------------------
  // Exceptions
  //---------------------------------------
  public static A Throw<A>( Exception exn ) {
    throw exn;
  }
  public static A Error<A>( string message ) {
    throw new ErrorException( message );
  }

  public static A PatternMatchError<A>( string location, string definition )
  {
    throw new PatternMatchException(location,definition);
  }

  public static A Unreachable<A>()
  {
    throw new ErrorException( "unreachable code reached");
  }

  public static A Catch<A>( Fun0<A> action, Fun1<Exception,A> handler )
  {
    try
    {
      return (A)action.Apply();
    }
    catch (Exception exn)
    {
      return (A)handler.Apply(exn);
    }
  }

  public static A Finally<A>( Fun0<A> action, Fun0<Unit> handler )
  {
    try
    {
      return (A)action.Apply();
    }
    finally
    {
      handler.Apply();
    }
  }

  //---------------------------------------
  // Run a stateful action safely
  //---------------------------------------
  public static A Run<A>( TypeFun1 action )
  {
    return (A)(((Fun0<A>)(action.TypeApply<Unit>())).Apply());
  }

  //---------------------------------------
  // Arrays
  //---------------------------------------
  public static A[] NewArray<A>( int len, A init )
  {
    A[] a = new A[len];
    for( int i = 0; i < len; i++) { a[i] = init; }
    return a;
  }

  //---------------------------------------
  // Dictionary
  //---------------------------------------
  public class Dict<T> : System.Collections.Generic.Dictionary<string,T> {
    public Dict() : base() {}
    public Dict( System.Collections.Generic.IDictionary<string,T> d ) : base(d) {}    
  }

  public static Dict<string> DictFromStringCollection( System.Collections.IDictionary d ) {
    Dict<string> dict = new Dict<string>();
    foreach( object key in d.Keys ) {
      if (key != null && key is string) {
        object val = d[key];
        if (val is string) {
          dict[(string)key] = (string)val;
        }
      }
    }
    return dict;
  }

  public class MDict<H,T> : System.Collections.Generic.Dictionary<string,T> {
    public MDict() : base() {}
    public MDict( System.Collections.Generic.IDictionary<string,T> d ) : base(d) {}
  }

  public static string[] DictKeys<A>( System.Collections.Generic.IDictionary<string,A> d ) {
    int i = 0;
    string[] result = new string[d.Keys.Count];
    foreach( string key in d.Keys) {
      result[i] = key;
      i++;
    }
    return result;
  }

  //---------------------------------------
  // Random
  //---------------------------------------
  private static Random random = new Random();
  public static double RandomDouble() 
  {
    return random.NextDouble();
  }

  public static int RandomInt()
  {
    return random.Next();
  }

  //---------------------------------------
  // Strings
  //---------------------------------------
  public static string Concat( string[] xs, string sep ) 
  {
    if (xs==null) return "";
    if (xs.Length==0) return "";
    StringBuilder sb = new StringBuilder(xs[0]);
    for (int i = 1; i < xs.Length; i++) {
      sb.Append(sep);
      sb.Append(xs[i]);
    }
    return sb.ToString();
  }

  //---------------------------------------
  // Trace
  //---------------------------------------
  public static void Trace( string msg ) 
  {
    System.Diagnostics.Debug.Print(msg);
    Console.Error.WriteLine(msg);
  }
  
  //---------------------------------------
  // ReadLine
  //---------------------------------------
  private static Async<string> onReadLine = null;

  public static Async<string> ReadLine()
  {
    if (onReadLine == null) onReadLine = new Async<string>();
    return onReadLine;
  }

  // For now, the MainConsole enters an event loop that handles
  // ReadLine from the Console. Later other asynchronous api's can
  // be added through the AsyncGlobal class, keeping the application
  // active as long as there are 'on' handlers installed.
  public static A MainConsole<A>( Fun0<A> f ) {
    A x = (A)f.Apply();
    while (!AsyncGlobal.AllDone()) {
      if (onReadLine != null) {
        string s = Console.In.ReadLine();
        Async<string> res = onReadLine;
        onReadLine = null;
        res.Supply(s);  // this may set onReadLine again
      }
      else {
        // bad
        Primitive.Trace("MainConsole: active async's but no readline");
        break;
      }
    }
    return x;
  }
};
  
//---------------------------------------
// Async
//---------------------------------------
public class AsyncGlobal 
{
  protected static int active = 0;

  public static bool AllDone() {
    return (active <= 0);
  }
}

public class Async<A> : AsyncGlobal
{
  Action<A> on = null;
  Action<Exception> onexn = null;
  bool done = false;
  Exception exn = null;
  A value;


  public bool IsDone {
    get { return done; }
  }

  public Async<B> On<B>( Fun1<A,B> fun ) {
    Async<B> result = new Async<B>();
    if (done) {
      if (exn == null) {
        result.Supply((B)fun.Apply(value));  
      }
    }
    else {
      if (on != null) {
        Action<A> prev = on;
        on = delegate(A x) { prev(x); result.Supply((B)fun.Apply(x)); };
      }
      else {
        active++;
        on = delegate(A x) { result.Supply((B)fun.Apply(x)); };
      }
    }
    return result;
  }

  public Async<B> OnExn<B>( Fun1<Exception,B> fun ) {
    Async<B> result = new Async<B>();
    if (done) {
      if (exn != null) {
         result.Supply((B)fun.Apply(exn));  
      }
    }
    if (onexn != null) {
      Action<Exception> prev = onexn;
      onexn = delegate(Exception x) { prev(x); result.Supply((B)fun.Apply(x)); };
    }
    else {
      active++;
      onexn = delegate(Exception x) { result.Supply((B)fun.Apply(x)); };
    }
    return result;
  }

  public void Supply( A x ) {
    if (done) return;
    done = true;
    value = x;
    if (on != null) {
      on(x);
      on = null; 
      active--;
    }
    if (onexn != null) {
      onexn = null;
      active--;
    }
  }

  public void SupplyExn( Exception x ) {
    if (done) return;
    done = true;
    exn = x;
    if (onexn != null) {
      onexn(exn);
      onexn = null; 
      active--;
    }
    if (on != null) {
      on = null;
      active--;
    }
  }

}


//---------------------------------------
// Exceptions classes
//---------------------------------------
public class ErrorException : Exception
{
  public ErrorException( string message ) : base(message) 
  {}
};

public class PatternMatchException : Exception
{
   public PatternMatchException( string location, string definition ) 
     : base( location + (String.IsNullOrEmpty(definition) ? "" : (": " + definition)) + ": pattern match failure" ) 
   { }
};

//---------------------------------------
// References
//---------------------------------------
public sealed class _Ref { }
public sealed class Ref<H,T> : TA<TA<_Ref,H>,T>
{
  public T Value;
  
  public Ref( T value ) {
    this.Value = value;
  }

  public void Set( T value ) {
    this.Value = value;
  }
}

//---------------------------------------
// Primitive types
//---------------------------------------

public enum Unit {
  unit
}

public interface TA<A,B>
{
}

public interface TypeFun1
{
  object TypeApply<A>();
}

public interface TypeFun2
{
  object TypeApply<A,B>();
}

public interface TypeFun3
{
  object TypeApply<A,B,C>();
}

public interface TypeFun4
{
  object TypeApply<A,B,C,D>();
}

public interface TypeFun5
{
  object TypeApply<A,B,C,D,E>();
}

public interface Fun0<in A> 
{
   object Apply();
}

public interface Fun1<in A, in B> 
{
  object Apply( A x );
}

public interface Fun2<in A1,in A2, in B> 
{
  object Apply( A1 x1, A2 x2 );
}

public interface Fun3<in A1, in A2, in A3, in B> 
{
  object Apply( A1 x1, A2 x2, A3 x3 );
}

public interface Fun4<in A1,in A2,in A3,in A4,in B> 
{
  object Apply( A1 x1, A2 x2, A3 x3, A4 x4 );
}

public interface Fun5<in A1,in A2,in A3,in A4,in A5, in B> 
{
  object Apply( A1 x1, A2 x2, A3 x3, A4 x4, A5 x5 );
}
