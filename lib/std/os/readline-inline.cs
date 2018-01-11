/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class _Readline
{
  public static Primitive.EventloopEntry Readline( Fun2<Exception,string,Unit> cb ) {
    return Primitive.RunBlocking<string>( 
      () => { return Console.In.ReadLine(); },
      (Exception exn,string result) => { cb.Apply(exn,result); }
    );
  }

  public static void CancelReadline( object obj ) {
    IDisposable d = obj as IDisposable;
    if (d != null) d.Dispose(); 
  }
}
