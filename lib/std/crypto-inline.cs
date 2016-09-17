/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class Crypto
{
  public static string HashMD5(string input) {
    // Use input string to calculate MD5 hash
    using (System.Security.Cryptography.MD5 md5 = System.Security.Cryptography.MD5.Create()) {
      System.Text.Encoding enc = new System.Text.UTF8Encoding(false,false);
      byte[] hash = md5.ComputeHash(enc.GetBytes(input));
      System.Text.StringBuilder sb = new System.Text.StringBuilder();
      for (int i = 0; i < hash.Length; i++) {
        sb.Append(hash[i].ToString("x2"));
      }
      return sb.ToString();
    }
  }
}
