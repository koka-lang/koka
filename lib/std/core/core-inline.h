






/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

kk_box_t kk_std_core_error_pattern(kk_string_t location, kk_string_t definition, kk_context_t* _ctx);

static inline kk_std_core_types__order kk_int_as_order(int i,kk_context_t* ctx) {
  return (i==0 ? kk_std_core_types__new_Eq(ctx) : (i > 0 ? kk_std_core_types__new_Gt(ctx) : kk_std_core_types__new_Lt(ctx)));
}

static inline kk_std_core_types__maybe kk_integer_xparse( kk_string_t s, bool hex, kk_context_t* ctx ) {
  kk_integer_t i;
  bool ok = (hex ? kk_integer_hex_parse(kk_string_cbuf_borrow(s),&i,ctx) : kk_integer_parse(kk_string_cbuf_borrow(s),&i,ctx) );
  kk_string_drop(s,ctx);
  return (ok ? kk_std_core_types__new_Just(kk_integer_box(i),ctx) : kk_std_core_types__new_Nothing(ctx));
}

struct kk_std_core_Sslice;

kk_datatype_t kk_string_to_list(kk_string_t s, kk_context_t* ctx);
kk_string_t   kk_string_from_list(kk_datatype_t cs, kk_context_t* ctx);

kk_datatype_t  kk_vector_to_list(kk_vector_t v, kk_datatype_t tail, kk_context_t* ctx);
kk_vector_t    kk_list_to_vector(kk_datatype_t xs, kk_context_t* ctx);

static inline kk_integer_t  kk_string_count_int(kk_string_t s, kk_context_t* ctx) {
  return kk_integer_from_size_t( kk_string_count(s), ctx );
}

static inline kk_integer_t kk_string_cmp_int(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return kk_integer_from_small( kk_string_cmp(s1,s2,ctx) );
}

kk_string_t  kk_string_join(kk_vector_t v, kk_context_t* ctx);
kk_string_t  kk_string_join_with(kk_vector_t v, kk_string_t sep, kk_context_t* ctx);
kk_string_t  kk_string_replace_all(kk_string_t str, kk_string_t pattern, kk_string_t repl, kk_context_t* ctx);
static inline kk_integer_t kk_string_count_pattern(kk_string_t str, kk_string_t pattern, kk_context_t* ctx) {
  kk_integer_t count = kk_integer_from_size_t( kk_string_count_pattern_borrow(str,pattern), ctx );
  kk_string_drop(str,ctx);
  kk_string_drop(pattern,ctx);
  return count;
}

kk_integer_t kk_slice_count( struct kk_std_core_Sslice sslice, kk_context_t* ctx );
kk_string_t  kk_slice_to_string( struct kk_std_core_Sslice sslice, kk_context_t* ctx );
struct kk_std_core_Sslice kk_slice_first( kk_string_t str, kk_context_t* ctx );
struct kk_std_core_Sslice kk_slice_last( kk_string_t str, kk_context_t* ctx );

struct kk_std_core_Sslice kk_slice_common_prefix( kk_string_t str1, kk_string_t str2, kk_integer_t upto, kk_context_t* ctx );
struct kk_std_core_Sslice kk_slice_advance( struct kk_std_core_Sslice slice, kk_integer_t count, kk_context_t* ctx );
struct kk_std_core_Sslice kk_slice_extend( struct kk_std_core_Sslice slice, kk_integer_t count, kk_context_t* ctx );
kk_std_core_types__maybe kk_slice_next( struct kk_std_core_Sslice slice, kk_context_t* ctx );


static inline kk_unit_t kk_vector_unsafe_assign( kk_vector_t v, size_t i, kk_box_t x, kk_context_t* ctx  ) {
  size_t len;
  kk_box_t* p = kk_vector_buf(v,&len);
  kk_assert(i < len);
  p[i] = x;
  kk_vector_drop(v,ctx); // TODO: use borrowing
  return kk_Unit;
}

kk_vector_t kk_vector_init( size_t n, kk_function_t init, kk_context_t* ctx);

static inline kk_vector_t kk_vector_allocz( size_t n, kk_context_t* ctx ) {
  return kk_vector_alloc( n, kk_box_null, ctx);
}

static inline kk_box_t kk_vector_at_int( kk_vector_t v, kk_integer_t n, kk_context_t* ctx ) {
  // TODO: check bounds
  return kk_vector_at(v,kk_integer_clamp_size_t(n,ctx));
}

static inline double kk_double_abs(double d) {
  return (isfinite(d) && d < 0.0 ? -d : d);
}

static inline kk_std_core_types__tuple2_ kk_integer_div_mod_tuple(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_integer_t mod;
  kk_integer_t div = kk_integer_div_mod(x,y,&mod,ctx);
  return kk_std_core_types__new_dash__lp__comma__rp_(kk_integer_box(div),kk_integer_box(mod),ctx);
}

kk_box_t kk_main_console( kk_function_t action, kk_context_t* ctx );

struct kk_std_core_error_s;
struct kk_std_core_error_s kk_error_from_errno( int err, kk_box_t result, kk_context_t* ctx );
