/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

using System.Collections.Generic;

public static class Atom {
  static int unique;
  static Dictionary<string,int> atoms = new Dictionary<string,int>();
  static Dictionary<int,string> atoms_rev = new Dictionary<int,string>();

  public static int Create( string s ) {
    if (atoms.ContainsKey(s)) return atoms[s];
    int i = unique++;
    atoms[s] = i;
    atoms_rev[i] = s;
    return i;
  }

  public static string ToString( int i ) {
    return atoms_rev[i];
  }

  public static Dictionary<string,bool> CreateMatcher( string[] xs, int ignoreCase) {
    Dictionary<string,bool> dict = new Dictionary<string,bool>(xs.Length);
    foreach(string s in xs) {
      if (ignoreCase != 0) dict[s.ToLower()] = true;
                      else dict[s] = true;
    }
    return dict;
  }

  public static int Match( object obj, string s, int ignoreCase ) {
    Dictionary<string,bool> dict = (Dictionary<string,bool>)(obj);
    if (ignoreCase != 0) s = s.ToLower();
    return (dict.ContainsKey(s) ? 1 : 0);
  }
}
