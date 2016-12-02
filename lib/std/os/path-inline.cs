/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class _Path
{
  public static string GetHomeDir() {
    string home = Environment.GetEnvironmentVariable("HOME");
    if (String.IsNullOrEmpty(home)) home = Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%");
    return home;
  }
}
