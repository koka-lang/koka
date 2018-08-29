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
  public static Primitive.EventloopEntry WriteTextFile( string path, string content, Fun1<Exception,Unit> cb ) {
    return Primitive.RunBlocking<string>( () => File.WriteAllText(path,content,Encoding.UTF8), cb );
  }
  public static Primitive.EventloopEntry GetModTime( string path, Fun2<Exception,double,Unit> cb ) {
    return Primitive.RunBlocking<double>( () => File.GetLastWriteTimeUtc(path).ToUnixSeconds(), cb);
  }
  public static Primitive.EventloopEntry SetAccessModTime( string path, double atime, double mtime, Fun1<Exception,Unit> cb ) {
    return Primitive.RunBlocking<string>( () => {
        File.SetLastAccessTimeUtc(path, Primitive.FromUnixSeconds(atime));
        File.SetLastWriteTimeUtc(path, Primitive.FromUnixSeconds(mtime));
      }, cb);
  }

  public class _DownloadText : IDisposable 
  {
    Primitive.EventloopEntry entry;
    System.Net.WebClient webClient;

    public _DownloadText( string uri, double timeout, Fun2<Exception,string,Unit> cb ) {
      entry = Primitive.GetEventloopEntry();
      webClient = new System.Net.WebClient();
      try {
        webClient.DownloadStringCompleted += (object sender, System.Net.DownloadStringCompletedEventArgs e) => {
          if (entry != null) {
            entry.Post( () => cb.Call(e.Error,(e.Error != null ? "" : e.Result)) );
            entry = null;
          }
          Dispose();
        };
        webClient.DownloadStringAsync(new Uri(uri));
      }
      catch(Exception exn) {
        Dispose();
        throw exn;
      }
    }

    public void Dispose() {
      Dispose(true);
    }

    protected virtual void Dispose(bool disposing) {
      if (!disposing) return;
      if (webClient != null) {
        if (entry != null) webClient.CancelAsync();
        webClient.Dispose();
        webClient = null;
      }
      if (entry != null) {
        entry.Dispose();
        entry = null;
      }
    }
  }

  public static _DownloadText DownloadText( string uri, double timeout, Fun2<Exception,string,Unit> cb ) {
    return new _DownloadText(uri,timeout,cb);
  }

  public static void CancelEntry( object obj ) {
    IDisposable d = obj as IDisposable;
    if (d != null) d.Dispose();
  }
}
