/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core__error kk_os_run_command_error( kk_string_t cmd, kk_context_t* ctx ) {
  kk_string_t output;
  const int err = kk_os_run_command(cmd,&output,ctx);
  if (err != 0) return kk_error_from_errno(err,ctx);
           else return kk_error_ok(kk_string_box(output),ctx);
}

static kk_integer_t kk_os_run_system_prim( kk_string_t cmd, kk_context_t* ctx ) {
  const int exitcode = kk_os_run_system(cmd,ctx);
  return kk_integer_from_int(exitcode,ctx);
}
