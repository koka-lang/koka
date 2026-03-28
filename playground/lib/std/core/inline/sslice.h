






/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

struct kk_std_core_sslice_Sslice;

kk_integer_t kk_slice_count( struct kk_std_core_sslice_Sslice sslice, kk_context_t* ctx );
kk_string_t  kk_slice_to_string( struct kk_std_core_sslice_Sslice sslice, kk_context_t* ctx );
struct kk_std_core_sslice_Sslice kk_slice_first( kk_string_t str, kk_context_t* ctx );
struct kk_std_core_sslice_Sslice kk_slice_last( kk_string_t str, kk_context_t* ctx );

struct kk_std_core_sslice_Sslice kk_slice_common_prefix_borrow( kk_string_t str1, kk_string_t str2, kk_integer_t upto, kk_context_t* ctx );
struct kk_std_core_sslice_Sslice kk_slice_advance_borrow( struct kk_std_core_sslice_Sslice slice, kk_integer_t count, kk_context_t* ctx );
struct kk_std_core_sslice_Sslice kk_slice_extend_borrow( struct kk_std_core_sslice_Sslice slice, kk_integer_t count, kk_context_t* ctx );
kk_std_core_types__maybe kk_slice_next( struct kk_std_core_sslice_Sslice slice, kk_context_t* ctx );
