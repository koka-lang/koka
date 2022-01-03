#pragma once
#ifndef KK_OS_H
#define KK_OS_H
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  
--------------------------------------------------------------------------------------*/

kk_decl_export kk_string_t kk_os_app_path(kk_context_t* ctx);
kk_decl_export kk_string_t kk_os_realpath(kk_string_t fname, kk_context_t* ctx);
kk_decl_export kk_string_t kk_os_path_sep(kk_context_t* ctx);
kk_decl_export kk_string_t kk_os_dir_sep(kk_context_t* ctx);
kk_decl_export kk_string_t kk_os_home_dir(kk_context_t* ctx);
kk_decl_export kk_string_t kk_os_temp_dir(kk_context_t* ctx);
kk_decl_export kk_vector_t kk_os_get_argv(kk_context_t* ctx);
kk_decl_export kk_vector_t kk_os_get_env(kk_context_t* ctx);

kk_decl_export int  kk_os_read_line(kk_string_t* result, kk_context_t* ctx);
kk_decl_export int  kk_os_read_text_file(kk_string_t path, kk_string_t* result, kk_context_t* ctx);
kk_decl_export int  kk_os_write_text_file(kk_string_t path, kk_string_t content, kk_context_t* ctx);

kk_decl_export int  kk_os_ensure_dir(kk_string_t dir, int mode, kk_context_t* ctx);
kk_decl_export int  kk_os_copy_file(kk_string_t from, kk_string_t to, bool preserve_mtime, kk_context_t* ctx);
kk_decl_export bool kk_os_is_directory(kk_string_t path, kk_context_t* ctx);
kk_decl_export bool kk_os_is_file(kk_string_t path, kk_context_t* ctx);
kk_decl_export int  kk_os_list_directory(kk_string_t dir, kk_vector_t* contents, kk_context_t* ctx);

kk_decl_export int  kk_os_run_command(kk_string_t cmd, kk_string_t* output, kk_context_t* ctx);
kk_decl_export int  kk_os_run_system(kk_string_t cmd, kk_context_t* ctx);

kk_decl_export kk_secs_t  kk_timer_ticks(kk_asecs_t* atto_secs, kk_context_t* ctx);
kk_decl_export kk_asecs_t kk_timer_resolution(kk_context_t* ctx);

kk_decl_export kk_secs_t  kk_time_unix_now(kk_asecs_t* atto_secs, kk_context_t* ctx);
kk_decl_export kk_asecs_t kk_time_resolution(kk_context_t* ctx);

kk_decl_export kk_string_t kk_compiler_version(kk_context_t* ctx);
kk_decl_export kk_string_t kk_cc_name(kk_context_t* ctx);
kk_decl_export kk_string_t kk_os_name(kk_context_t* ctx);
kk_decl_export kk_string_t kk_cpu_arch(kk_context_t* ctx);
kk_decl_export int         kk_cpu_count(kk_context_t* ctx);
kk_decl_export bool        kk_cpu_is_little_endian(kk_context_t* ctx);

kk_decl_export bool kk_os_set_stack_size( kk_ssize_t stack_size );

#endif // include guard
