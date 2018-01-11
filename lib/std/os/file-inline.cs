/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
using System.IO;
using System.Text;

static class _File
{ 
  public static Primitive.EventloopEntry ReadTextFile( string path, Fun2<Exception,string,Unit> cb ) {
    return Primitive.RunBlocking<string>( () => File.ReadAllText(path,Encoding.UTF8), cb );
  }

  public static void CancelEntry( object obj ) {
    Primitive.EventloopEntry entry = obj as Primitive.EventloopEntry;
    if (entry == null) return;
    entry.Close();
  }
}
