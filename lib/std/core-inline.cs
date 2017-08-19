/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
using System.Text;
using System.Collections.Generic;
using System.Globalization;             // CultureInfo.InvariantCulture
using System.Text.RegularExpressions;   // Parsing BigInteger's & showing doubles

public static class Primitive
{
  //---------------------------------------
  // Exceptions
  //---------------------------------------
  public static A Throw<A>( Exception exn ) {
    throw exn;
  }

  public static A ExnErrorPattern<A>( string location, string definition )
  {
    string msg = location + (String.IsNullOrEmpty(definition) ? "" : (": " + definition)) + ": pattern match failure";
    throw new InfoException( msg, new std_core.Pattern_(location,definition) );
  }

  public static InfoException ExnException( string message, std_core._exception_info info ) {
    return new InfoException(message,info);
  }

  public static std_core._exception_info ExnInfo( Exception exn ) {
    InfoException eexn = exn as InfoException;
    if (eexn != null) {
      return eexn.info;
    }
    else {
      return new std_core.Error_();
    }
  }

  public static string ExnMessage( Exception exn ) {
    return (exn != null ? exn.Message : "invalid error");
  }

  public static string ExnStackTrace( Exception exn ) {
    return (exn != null ? exn.StackTrace : "");
  }



  public static A Unreachable<A>()
  {
    throw new InfoException( "unreachable code reached", new std_core.Error_() );
  }

  public static A UnsupportedExternal<A>( string name ) {
    throw new InfoException( "external '" + name + "' is not supported on this platform", new std_core.Error_() );
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

  public static B YieldOp<A,B>( std_core._operation<A> op, Fun1<B,B> _k ) {
    throw new YieldException<A,B>( op, _k );
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

  public static A[] NewArray<A>( int len, Fun1<int,A> init )
  {
    A[] a = new A[len];
    for( int i = 0; i < len; i++) {
      a[i] = (A)init.Apply(i);
    }
    return a;
  }

  public static std_core._list<A> VList<A>( A[] v, std_core._list<A> tail ) {
    std_core._list<A> xs = tail;
    for(int i = v.Length-1; i >= 0; i--) {
      xs = new std_core._list<A>( v[i], xs );
    }
    return xs;
  }

  public static A[] UnVList<A>( std_core._list<A> xs ) {
    int len = 0;
    std_core._list<A> acc = xs;
    while(acc != std_core._list<A>.Nil_) {
      len++;
      acc = acc.tail;
    }
    A[] v = new A[len];
    acc = xs;
    for(int i = 0; i < len; i++) {
      v[i] = acc.head;
      acc = acc.tail;
    }
    return v;
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

  public static int Count( string s, string pattern )
  {
    if (String.IsNullOrEmpty(pattern)) return 0;
    int count = 0;
    int i = 0;
    while( (i = s.IndexOf(pattern,i)) > 0 ) {
      count++;
    }
    return count;
  }

  public static string Repeat( string s, int n ) {
    if (n <= 0 || String.IsNullOrEmpty(s)) return "";
    StringBuilder sb = new StringBuilder("");
    for(int i = 0; i < n; i++) {
      sb.Append(s);
    }
    return sb.ToString();
  }

  public static std_core._list<int> StringToList( string s ) {
    std_core._list<int> xs = std_core._list<int>.Nil_;
    for(int i = s.Length-1; i >= 0; i--) {
      if (Char.IsLowSurrogate(s[i]) && i > 0) i--;
      xs = new std_core._list<int>( Char.ConvertToUtf32(s,i), xs );
    }
    return xs;
  }

  public static int[] StringToChars( string s ) {
    List<int> v = new List<int>(s.Length);
    for(int i = 0; i < s.Length; i++) {
      v.Add( Char.ConvertToUtf32(s,i) );
      if (Char.IsHighSurrogate(s[i])) i += 1;
    }
    return v.ToArray();
  }

  public static BigInteger StringCount( string s ) {
    int n = 0;
    for(int i = 0; i < s.Length; i++) {
      n++;
      if (Char.IsHighSurrogate(s[i])) i+=1;
    }
    return new BigInteger(n);
  }

  public static string CharToString( int c ) {
    return Char.ConvertFromUtf32(c);
  }

  public static string CharsToString( int[] v ) {
    StringBuilder sb = new StringBuilder();
    foreach( int c in v) {
      sb.Append( CharToString(c) );
    }
    return sb.ToString();
  }

  public static string ListToString( std_core._list<int> xs ) {
    StringBuilder sb = new StringBuilder();
    while(xs != std_core._list<int>.Nil_) {
      sb.Append( CharToString(xs.head) );
      xs = xs.tail;
    }
    return sb.ToString();
  }

  public static std_core._sslice SliceFirst( string s ) {
    if (String.IsNullOrEmpty(s))
      return new std_core._sslice("",0,0);
    else
      return new std_core._sslice(s,0,Char.IsHighSurrogate(s[0]) ? 2 : 1);
  }

  public static std_core._sslice SliceLast( string s ) {
    if (String.IsNullOrEmpty(s))
      return new std_core._sslice("",0,0);
    else if (Char.IsLowSurrogate(s[s.Length-1]) && s.Length > 1)
      return new std_core._sslice(s,s.Length-2,2);
    else
      return new std_core._sslice(s,s.Length-1,1);
  }

  public static BigInteger SliceCount( std_core._sslice slice ) {
    int n = 0;
    for(int i = slice.start; i < slice.start + slice.len; i++) {
      n++;
      if (Char.IsHighSurrogate(slice.str[i])) i+=1;
    }
    return new BigInteger(n);
  }

  public static std_core._sslice SliceExtend( std_core._sslice slice, BigInteger bcount ) {
    int count = IntToInt32(bcount);
    if (count==0) return slice;
    int i = slice.start + slice.len;
    if (count > 0) {
      while(i < slice.str.Length && count > 0) {
        count--;
        i += (Char.IsHighSurrogate(slice.str[i]) && i < slice.str.Length-1 ? 2 : 1);
      }
    }
    else {
      while(i > slice.start && i > 0 && count < 0) {
        count++;
        i -= (Char.IsLowSurrogate(slice.str[i-1]) && i > slice.start+1 ? 2 : 1);
      }
    }
    return new std_core._sslice(slice.str, slice.start, (i > slice.start ? i - slice.start : 0));
  }

  public static std_core._sslice SliceAdvance( std_core._sslice slice, BigInteger bcount ) {
    int count = IntToInt32(bcount);
    if (count==0) return slice;
    int i   = slice.start;
    int end = slice.start + slice.len;
    int sliceCount = IntToInt32(SliceCount(slice));
    int extra = 0;
    if (count > 0) {
      while(i < slice.str.Length && extra < count) {
        extra++;
        i += (Char.IsHighSurrogate(slice.str[i]) && i < slice.str.Length-1 ? 2 : 1);
      }
      if (end > i && sliceCount > extra) {
        return SliceExtend( new std_core._sslice(slice.str, i, end - i), extra );
      }
    }
    else {
      while(i > 0 && extra < -count) {
        extra++;
        i -= (Char.IsLowSurrogate(slice.str[i-1]) && i > 1 ? 2 : 1);
      }
      if (sliceCount > extra) {
        return SliceExtend( new std_core._sslice(slice.str, i, slice.start-i), sliceCount - extra );
      }
    }
    return SliceExtend( new std_core._sslice(slice.str, i, 0), sliceCount );
  }


  public static string SliceToString( std_core._sslice slice ) {
    if (slice.start==0 && slice.len==slice.str.Length) return slice.str;
    return slice.str.Substring(slice.start,slice.len);
  }

  public static std_core._sslice SliceCommonPrefix( string s, string t, BigInteger bupto ) {
    int upto = IntToInt32(bupto);
    int min  = Math.Min(s.Length,t.Length);
    int i;
    if (upto<0) upto = min;
    for(i = 0; i<min && upto>0; i++) {
      if (s[i] != t[i]) break;
      if (!Char.IsLowSurrogate(s[i])) upto--;
    }
    return new std_core._sslice(s,0,i);
  }

  public static std_core._maybe<std_core._Tuple2_<int,std_core._sslice>> SliceNext( std_core._sslice slice ) {
    if (slice.len <= 0) return std_core._maybe<std_core._Tuple2_<int,std_core._sslice>>.Nothing_;
    char c = slice.str[slice.start];
    int n = 1;
    if (Char.IsHighSurrogate(c) && slice.len > 1) {
      char lo = slice.str[slice.start+1];
      if (Char.IsLowSurrogate(lo)) {
        c = (char)Char.ConvertToUtf32(slice.str,slice.start);
        n = 2;
      }
    }
    return new std_core._maybe<std_core._Tuple2_<int,std_core._sslice>>(
                  new std_core._Tuple2_<int,std_core._sslice>(
                    (int)c, new std_core._sslice(slice.str, slice.start+n, slice.len-n ) ) );
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

  //---------------------------------------
  // Integers
  //---------------------------------------
  public static BigInteger IntString(string s) {
    BigInteger i;
    bool ok = BigInteger.TryParse(s,out i);
    return (ok ? i : BigInteger.Zero);
  }

  public static BigInteger IntDouble(double d) {
    return new BigInteger(d);
  }

  public static double IntToDouble(BigInteger i) {
    return (double)(i);
  }

  public static int DoubleToInt32( double d ) {
    if (d > Int32.MaxValue) return Int32.MaxValue;
    if (d < Int32.MinValue) return Int32.MinValue;
    if (Double.IsNaN(d))    return 0;
    return Convert.ToInt32(d);
  }

  public static int IntToInt32(BigInteger i) {
    if (i<Int32.MinValue) return Int32.MinValue;
    if (i>Int32.MaxValue) return Int32.MaxValue;
    return (int)(i);
  }

  public static std_core._order IntCompare( BigInteger i, BigInteger j ) {
    int s = BigInteger.Compare(i,j);
    return (s<0 ? std_core._order.Lt : (s>0 ? std_core._order.Gt : std_core._order.Eq));
  }

  public static std_core._order IntSign( BigInteger i ) {
    int s = i.Sign;
    return (s<0 ? std_core._order.Lt : (s>0 ? std_core._order.Gt : std_core._order.Eq));
  }

  public static std_core._Tuple2_<BigInteger,BigInteger> IntDivMod(BigInteger i, BigInteger j) {
    if (j.IsZero) return new std_core._Tuple2_<BigInteger,BigInteger>(BigInteger.Zero,BigInteger.Zero);
    BigInteger r;
    BigInteger q = BigInteger.DivRem(i,j,out r);
    if (r.Sign<0) {
      if (j.Sign>0) { q = q-1; r = r + j; }
              else  { q = q+1; r = r - j; }
    }
    return new std_core._Tuple2_<BigInteger,BigInteger>(q,r);
  }

  public static BigInteger IntDiv(BigInteger i, BigInteger j) {
    if (j.IsZero) return BigInteger.Zero;
    BigInteger r;
    BigInteger q = BigInteger.DivRem(i,j,out r);
    return (r.Sign<0 ? (j.Sign>0 ? q-1 : q+1) : q);
  }

  public static BigInteger IntMod(BigInteger i, BigInteger j) {
    if (j.IsZero) return BigInteger.Zero;
    BigInteger r = BigInteger.Remainder(i,j);
    return (r.Sign<0 ? (j.Sign>0 ? r+j : r-j) : r);
  }

  public static String IntShowHex(BigInteger i, bool useCapitals) {
    string s = i.ToString( (useCapitals ? "X" : "x" )).TrimStart('0');
    return (String.IsNullOrEmpty(s) ? "0" : s);
  }

  public static std_core._maybe<BigInteger> IntParse( string s, bool hex ) {
    Regex rxpre = new Regex(@"^([\-\+])?(?:(0[xX][\da-fA-F]+)|(\d+)(?:[eE]?([\-\+]?\d+))?)$"); //todo: allow hex
    Match mpre  = rxpre.Match(s);
    string sign = (mpre.Groups[1].Value=="-" ? "-" : "");
    string sdigits = (String.IsNullOrEmpty(mpre.Groups[2].Value) ? (hex ? "0x" : "") + mpre.Groups[3].Value : mpre.Groups[2].Value);
    BigInteger res = 0;
    bool ok = BigInteger.TryParse( sign + sdigits, out res);
    if (ok && !String.IsNullOrEmpty(mpre.Groups[4].Value)) {
      int exp = 0;
      ok = int.TryParse(mpre.Groups[4].Value, out exp);
      res = (ok ? (exp >= 0 ? res * BigInteger.Pow(10,exp) : res / BigInteger.Pow(10,-exp)): 0);
    }
    return (ok ? new std_core._maybe<BigInteger>(res) : std_core._maybe<BigInteger>.Nothing_ );
  }

  public static BigInteger IntCountDigits( BigInteger i ) {
    double d = BigInteger.Log10(BigInteger.Abs(i));
    return new BigInteger(Math.Ceiling(d));
  }

  public static BigInteger IntCountPow10( BigInteger i ) {
    BigInteger x = BigInteger.Abs(i);
    BigInteger r = 0;
    int n = 0;
    while(r == 0 && x > 0) {
      x = BigInteger.DivRem(x,10,out r);
      if (r==0) n++;
    }
    return n;
  }

	public static BigInteger IntPow( BigInteger i, BigInteger exp ) {
		return BigInteger.Pow(i,(int)exp);
	}

  public static BigInteger IntCDivPow10( BigInteger i, BigInteger n ) {
    if (n < 0) return IntMulPow10(i,-n);
          else return i / BigInteger.Pow(10,(int)n);
  }

  public static BigInteger IntMulPow10( BigInteger i, BigInteger n ) {
    if (n < 0) return IntCDivPow10(i,-n);
          else return i * BigInteger.Pow(10,(int)n);
  }

  public static double DoubleParse( string s ) {
    double res;
    bool ok = Double.TryParse( s, out res);
    return (ok ? res : Double.NaN );
  }

  public static bool DoubleIsFinite( double d ) {
    return (!double.IsNaN(d) && !double.IsInfinity(d));
  }

  private static string DoubleNormalizeExp( string s ) {
    return Regex.Replace(s,@"[eE]([\+\-]?)0*(\d+)$","e$1$2");  // remove zeros from exponent
  }

  public static string DoubleShowExp( double d, int fractionDigits ) {
    if (!DoubleIsFinite(d)) {
      return d.ToString(CultureInfo.InvariantCulture);
    }
    else if (fractionDigits < 0) {
      // use at most |fractionDigits|, as needed
      fractionDigits = -fractionDigits;
      if (fractionDigits >= 15) {   // .net does not respect more than 15 '#'
        string s = DoubleNormalizeExp(d.ToString("E",CultureInfo.InvariantCulture));
        string nozeros = Regex.Replace(s,@"(?:\.0*|(\.\d*[1-9])0+)([eE]|$)","$1$2");
        return Regex.Replace(nozeros,@"[eE]\+0+$","");
      }
      else {
        if (fractionDigits > 20) fractionDigits = 20;
        string format = "0." + new String('#',fractionDigits) + "e+0";
        return DoubleNormalizeExp(d.ToString(format,CultureInfo.InvariantCulture));
      }
    }
    else {
      // use always |fractionDigits|
      if (fractionDigits < 0)  fractionDigits = -fractionDigits;
      if (fractionDigits > 20) fractionDigits = 20;
      string format = "E" + fractionDigits.ToString(); // "0." + new String('0',fractionDigits) + "e+0";
      string s = d.ToString(format,CultureInfo.InvariantCulture);
      return DoubleNormalizeExp(s);
    }
  }

  public static string DoubleShowFixed( double d, int fractionDigits ) {
    if (!DoubleIsFinite(d)) {
      return d.ToString(CultureInfo.InvariantCulture);
    }
    else if (d < 1.0e-15 || d > 1.0e+21) {
      return DoubleShowExp(d,fractionDigits);
    }
    else if (fractionDigits < 0) {
      // use at most |fractionDigits|, as needed
      fractionDigits = -fractionDigits;
      if (fractionDigits > 15) fractionDigits = 15; // .net does not respect more than 15 '#'
      string format = "0." + new String('#',fractionDigits);
      return d.ToString(format,CultureInfo.InvariantCulture);
    }
    else {
      // use always |fractionDigits|
      if (fractionDigits < 0) fractionDigits = -fractionDigits;
      if (fractionDigits > 20) fractionDigits = 20;
      string format = "F" + fractionDigits.ToString();
      return DoubleNormalizeExp(d.ToString(format, CultureInfo.InvariantCulture));
    }
  }

  public static double DoubleFromBits( int lo, int hi ) {
    ulong l = (((ulong)hi) << 32) | (uint)lo;
    return BitConverter.Int64BitsToDouble( (long)l );
  }

  public static std_core._Tuple2_<int,int> DoubleToBits( double d ) {
    ulong l = (ulong)BitConverter.DoubleToInt64Bits(d);
    return new std_core._Tuple2_<int,int>( (int)(l & 0xFFFFFFFFL), (int)(l >> 32) );
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
public class InfoException : Exception
{
  public readonly std_core._exception_info info;

  public InfoException( string message, std_core._exception_info info ) : base(message) {
    this.info = info;
  }
};


public class YieldException<A,B> : Exception
{
  public readonly std_core._operation<A> op;
  public readonly Fun1<B,B> k;

  public YieldException( std_core._operation<A> op, Fun1<B,B> k ) {
    this.op = op;
    this.k = k;
  }
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

public interface TypeFun6
{
  object TypeApply<A,B,C,D,E,F>();
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

public interface Fun6<in A1,in A2,in A3,in A4,in A5,in A6, in B>
{
  object Apply( A1 x1, A2 x2, A3 x3, A4 x4, A5 x5, A6 x6 );
}
