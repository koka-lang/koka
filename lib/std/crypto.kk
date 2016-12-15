/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Basic cryptography. 
*/
public module std/crypto

extern include {
  cs file "crypto-inline.cs"
  js file "crypto-inline.js"
}

// Compute an md5 hash of a string.
extern md5( s : string ) : string {
  cs "Crypto.HashMD5"
  js "_hash_md5"
}