/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

kk_box_t kk_std_core_error_pattern(kk_string_t location, kk_string_t definition, kk_context_t* _ctx);

typedef kk_std_core_types__result kk_std_core_exn__error;
kk_std_core_exn__error kk_error_ok( kk_box_t result, kk_context_t* ctx );
kk_std_core_exn__error kk_error_from_errno( int err, kk_context_t* ctx );