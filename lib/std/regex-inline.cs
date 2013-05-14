/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

using System.Text.RegularExpressions;

static class RegEx
{
  public static Regex Create( string s, int ignoreCase, int multiLine ) 
  {
    RegexOptions options = (ignoreCase != 0 ? RegexOptions.IgnoreCase : RegexOptions.None) |
                           (multiLine != 0 ? RegexOptions.Multiline : RegexOptions.None);
    return new Regex( s, options );                           
  }

  public static koka_text_regex._matched Matches( Match match ) 
  {
    if (!match.Success) return new koka_text_regex._matched( -1, "", null );    
    string[] groups = new string[match.Groups.Count];
    for( int i = 0; i < match.Groups.Count; i++) {
      groups[i] = (match.Groups[i].Success ? match.Groups[i].Value : "");
    }
    return new koka_text_regex._matched( match.Index, groups[0], groups );
  }

  public static koka_text_regex._matched Exec( object r, string s, int start ) {
    Regex regex = (Regex)(r);
    return Matches(regex.Match(s,start));
  }  

  public static koka_text_regex._matched[] ExecAll( object r, string s, int start ) {
    Regex regex = (Regex)(r);
    MatchCollection matches = regex.Matches(s,start);
    koka_text_regex._matched[] result = new koka_text_regex._matched[matches.Count];
    for (int i = 0; i < matches.Count; i++) {
      result[i] = Matches(matches[i]);
    }
    return result;
  }  

  public static string ReplaceFun( object r, string s, Fun1<koka_text_regex._matched,string> repl, int all, int start) 
  {
    Regex regex = (Regex)(r);
    int count = (all != 0 ? int.MaxValue : 1);
    return regex.Replace( s, delegate( Match match ) {
        if (!match.Success) return "";
                       else return (string)repl.Apply( Matches(match) );
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
