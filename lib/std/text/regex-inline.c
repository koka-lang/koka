/*---------------------------------------------------------------------------
  Copyright 2021 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#pragma clang diagnostic ignored "-Wdisabled-macro-expansion"
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

static kk_string_t kk_regex_version(kk_context_t* ctx) {
  char ver[256];
  int res = pcre2_config(PCRE2_CONFIG_VERSION, &ver);
  return kk_string_alloc_from_qutf8(ver, ctx);
}

