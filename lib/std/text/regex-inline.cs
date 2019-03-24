/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

using System.Text.RegularExpressions;

class KokaRegex : Regex {
  public string source;
  public bool alternative = false;
  public KokaRegex( string source, RegexOptions options ) : base(source,options) {
    this.source = source;
  } 
}

static class RegEx
{
  public static Regex CreateAlt( string[] xs, int ignoreCase, int multiLine ) {
    throw new Exception("regex.createAlt: not supported on .NET");
  }
  
  public static Regex Create( string s, int ignoreCase, int multiLine ) 
  {
    RegexOptions options = (ignoreCase != 0 ? RegexOptions.IgnoreCase : RegexOptions.None) |
                           (multiLine != 0 ? RegexOptions.Multiline : RegexOptions.None);
    return new KokaRegex( s, options );                           
  }

  public static string Source( object r ) {
    KokaRegex regex = (KokaRegex)(r);
    return regex.source;
  }

  public static string GroupsIndex( object g, int index ) {
    GroupCollection groups = (GroupCollection)(g);
    if (groups==null || index < 0 || index >= groups.Count) 
      return "";
    else 
      return groups[index].Value;
  }

  public static int GroupsMatchedOn( object g, int index ) {
    GroupCollection groups = (GroupCollection)(g);
    if (groups==null || index < 0 || index >= groups.Count || !groups[index].Success) 
      return 0;
    else 
      return 1;
  }

  public static __std_text_regex._matched Matches( string s, Match match ) 
  {
    if (!match.Success) return new __std_text_regex._matched( __std_core._new_sslice(s,0,0), "", new __std_text_regex._groups(null) );    
    int next = match.Index + match.Length;
    //if (next<=match.Index) next = match.Index+1;
    var slice = __std_core._new_sslice( s, match.Index, next - match.Index );
    return new __std_text_regex._matched( slice, match.Value, new __std_text_regex._groups(match.Groups) );
  }

  public static __std_core._maybe<__std_text_regex._matched> MaybeMatches( string s, Match match ) {
    if (!match.Success)
      return __std_core._maybe<__std_text_regex._matched>.Nothing_;
    else 
      return new __std_core._maybe<__std_text_regex._matched>( Matches(s, match) );
  }
  
  public static __std_core._maybe<__std_text_regex._matched> Exec( object r, string s, int start ) {
    Regex regex = (Regex)(r);
    return MaybeMatches(s, regex.Match(s,start));
  }  

  public static __std_text_regex._matched[] ExecAll( object r, string s, int start ) {
    Regex regex = (Regex)(r);
    MatchCollection matches = regex.Matches(s,start);
    __std_text_regex._matched[] result = new __std_text_regex._matched[matches.Count];
    for (int i = 0; i < matches.Count; i++) {
      result[i] = Matches(s, matches[i]);
    }
    return result;
  }  

  public static string ReplaceFun<E>( object r, string s, Fun1<__std_text_regex._matched,string> repl, int all, int start) 
  {
    Regex regex = (Regex)(r);
    int count = (all != 0 ? int.MaxValue : 1);
    return regex.Replace( s, delegate( Match match ) {
        if (!match.Success) return "";
                       else return (string)repl.Apply( Matches(s, match) );
      }, count, start);
  }

  public static string Replace( object r, string s, string repl, int all, int start) 
  {
    Regex regex = (Regex)(r);
    int count = (all != 0 ? int.MaxValue : 1);
    return regex.Replace( s, repl, count, start);
  }

  public static string[] Split( object r, string s, int n, int start )
  {
    Regex regex = (Regex)(r);
    return regex.Split( s, n, start ); 
  }

}
