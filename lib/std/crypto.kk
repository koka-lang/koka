/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Basic cryptography. 
*/
public module std/crypto

external include {
  js file "crypto-inline.js"
}
// Compute an md5 hash of a string.
external md5( s : string ) : string {
  js "$md5"
}