/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
using System.Threading;

static class _Async
{
  public class ThreadTimer : IDisposable {
    private Primitive.EventloopEntry entry;
    private Timer timer;

    public ThreadTimer( Fun0<Unit> cb, int ms ) {
      entry = Primitive.GetEventloopEntry();
      if (ms <= 0) {
        timer = null;
        entry.Post(() => { cb.Apply(); });
      }
      else {
        timer = new Timer( (object state0) => { 
                  if (entry != null) entry.Post(() => { cb.Apply(); }); 
                }, null, ms, Timeout.Infinite );
      }
    }

    public void Dispose() {
      Dispose(true);
    }

    protected virtual void Dispose(bool disposing) {
      if (entry != null) {
        entry.Dispose();
        entry = null;
      }
      if (timer != null) {
        timer.Dispose();
        timer = null;
      }
    }
  }

  public static ThreadTimer SetTimeout( Fun0<Unit> cb, int ms ) {
    return new ThreadTimer(cb,ms);
  }

  public static void ClearTimeout( object obj ) {
    IDisposable d = obj as IDisposable;
    if (d != null) d.Dispose();
  }
}
