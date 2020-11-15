/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core__error kk_os_read_text_file_error( kk_string_t path, kk_context_t* ctx ) {
  kk_string_t content;
  const int err = kk_os_read_text_file(path,&content,ctx);
  return kk_error_from_errno(err,kk_string_box(content),ctx);
}

static kk_std_core__error kk_os_write_text_file_error( kk_string_t path, kk_string_t content, kk_context_t* ctx ) {
  const int err = kk_os_write_text_file(path,content,ctx);
  return kk_error_from_errno(err,kk_unit_box(kk_Unit),ctx);
}

static kk_std_core__error kk_os_ensure_dir_error( kk_string_t path, kk_integer_t mode, kk_context_t* ctx ) {
  int m  = kk_integer_clamp32(mode,ctx);
  const int err = kk_os_ensure_dir(path,m,ctx);
  return kk_error_from_errno(err,kk_unit_box(kk_Unit),ctx);
}

static kk_std_core__error kk_os_copy_file_error( kk_string_t from, kk_string_t to, bool preserve_mtime, kk_context_t* ctx ) {
  const int err = kk_os_copy_file(from,to,preserve_mtime,ctx);
  return kk_error_from_errno(err,kk_unit_box(kk_Unit),ctx);
}

static kk_std_core__error kk_os_list_directory_prim( kk_string_t dir, kk_context_t* ctx ) {
  kk_vector_t contents;
  const int err = kk_os_list_directory(dir,&contents,ctx);
  return kk_error_from_errno(err,kk_vector_box(contents,ctx),ctx);
}

static kk_std_core__error kk_os_run_command_error( kk_string_t cmd, kk_context_t* ctx ) {
  kk_string_t output;
  const int err = kk_os_run_command(cmd,&output,ctx);
  return kk_error_from_errno(err,kk_string_box(output),ctx);
}

static kk_integer_t kk_os_run_system_prim( kk_string_t cmd, kk_context_t* ctx ) {
  const int exitcode = kk_os_run_system(cmd,ctx);
  return kk_integer_from_int(exitcode,ctx);
}
