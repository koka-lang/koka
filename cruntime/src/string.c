/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"

int_t string_cmp_borrow(string_t str1, string_t str2) {
  const char* s1 = string_buf(str1);
  const char* s2 = string_buf(str2);
  return strcmp(s1, s2);
}

int_t string_cmp(string_t str1, string_t str2, context_t* ctx) {
  int_t ord = string_cmp_borrow(str1,str2);
  string_drop(str1,ctx);
  string_drop(str2,ctx);
  return ord;
}

int_t string_icmp_borrow(string_t str1, string_t str2) {
  const char* s1 = string_buf(str1);
  const char* s2 = string_buf(str2);
  return _stricmp(s1, s2);
}

int_t string_icmp(string_t str1, string_t str2, context_t* ctx) {
  int_t ord = string_icmp_borrow(str1, str2);
  string_drop(str1, ctx);
  string_drop(str2, ctx);
  return ord;
}
