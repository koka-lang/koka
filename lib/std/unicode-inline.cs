/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
using System.Text;
using System.Globalization;

static class Unicode {
  public static string Normalize( string s, string norm ) {
    NormalizationForm nf;
    if (norm=="NFD") nf = NormalizationForm.FormD;
    else if (norm=="NFKC") nf = NormalizationForm.FormKC;
    else if (norm=="NFKD") nf = NormalizationForm.FormKD;
    else nf = NormalizationForm.FormC;
    return s.Normalize(nf);
  }
  public static string[] Graphemes( string s ) {
    if (String.IsNullOrEmpty(s)) return new string[0];
    string[] v = new string[s.Length];
    TextElementEnumerator e = StringInfo.GetTextElementEnumerator(s);
    int i = 0;
    while( e.MoveNext() ) {
      v[i] = (string)e.Current;
      i++;
    }
    Array.Resize(ref v,i);
    return v;
  }
}
