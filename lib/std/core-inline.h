/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

box_t __std_core_error_pattern(string_t location, string_t definition, context_t* _ctx);

static inline enum __std_core__order_e __std_core__new_Eq(context_t* _ctx);
static inline enum __std_core__order_e __std_core__new_Lt(context_t* _ctx);
static inline enum __std_core__order_e __std_core__new_Gt(context_t* _ctx);

static inline enum __std_core__order_e int_as_order(int i,context_t* ctx) {
  return (i==0 ? __std_core__new_Eq(ctx) : (i > 0 ? __std_core__new_Gt(ctx) : __std_core__new_Lt(ctx)));
}

static inline __std_core_types__maybe integer_xparse( string_t s, bool hex, context_t* ctx ) {
  if (hex) {
    unsupported_external("integer_xparse: hexadecimal");
    return __std_core_types__new_Nothing(ctx);
  }
  else {
    integer_t i = integer_parse(string_cbuf_borrow(s),ctx);
    string_drop(s,ctx);
    return (box_eq(i,box_null) ? __std_core_types__new_Nothing(ctx) : __std_core_types__new_Just(box_integer_t(i),ctx));
  }
}

datatype_t string_to_list(string_t s, context_t* ctx);
string_t   string_from_list(datatype_t cs, context_t* ctx);

datatype_t vector_to_list(vector_t v, datatype_t tail, context_t* ctx);
vector_t   list_to_vector(datatype_t xs, context_t* ctx);

static inline integer_t  string_count_int(string_t s, context_t* ctx) {
  return integer_from_uint_t( string_count(s), ctx );
}

static inline integer_t string_cmp_int(string_t s1, string_t s2, context_t* ctx) {
  return integer_from_small( string_cmp(s1,s2,ctx) );
}

static inline string_t string_repeat32(string_t s, int32_t n, context_t* ctx) {
  return string_repeat(s, n, ctx);
}

int32_t   string_index_of(string_t str, string_t sub, context_t* ctx);
int32_t   string_last_index_of(string_t str, string_t sub, context_t* ctx);
bool      string_starts_with(string_t str, string_t pre, context_t* ctx);
bool      string_ends_with(string_t str, string_t post, context_t* ctx);
bool      string_contains(string_t str, string_t sub, context_t* ctx);
string_t  string_join(vector_t v, context_t* ctx);
string_t  string_join_with(vector_t v, string_t sep, context_t* ctx);
string_t  string_replace_all(string_t str, string_t pattern, string_t repl, context_t* ctx);
integer_t string_count_pattern(string_t str, string_t pattern, context_t* ctx);
string_t  string_to_upper(string_t str, context_t* ctx);
string_t  string_to_lower(string_t strs, context_t* ctx);
string_t  string_trim_left(string_t strs, context_t* ctx);
string_t  string_trim_right(string_t strs, context_t* ctx);

integer_t slice_count( struct __std_core_Sslice sslice, context_t* ctx );
string_t  slice_to_string( struct __std_core_Sslice sslice, context_t* ctx );
struct __std_core_Sslice slice_first( string_t str, context_t* ctx );
struct __std_core_Sslice slice_last( string_t str, context_t* ctx );

// todo
struct __std_core_Sslice slice_common_prefix( string_t str1, string_t str2, integer_t upto, context_t* ctx );
struct __std_core_Sslice slice_advance( struct __std_core_Sslice slice, integer_t count, context_t* ctx );
struct __std_core_Sslice slice_extend( struct __std_core_Sslice slice, integer_t count, context_t* ctx );

__std_core_types__maybe slice_next( struct __std_core_Sslice slice, context_t* ctx );

static inline box_t vector_at32( vector_t v, int32_t i, context_t* ctx  ) {
  assert_internal(i >= 0);
  return vector_at(v,i);
}

static inline unit_t vector_unsafe_assign32( vector_t v, int32_t i, box_t x, context_t* ctx  ) {
  size_t len;
  box_t* p = vector_buf(v,&len);
  assert_internal(i >= 0 && i < len);
  p[i] = x;
  vector_drop(v,ctx); // TODO: avoid?
  return Unit;  
}

vector_t vector_init32( int32_t n, function_t init, context_t* ctx);
  
static inline vector_t vector_alloc32( int32_t n, context_t* ctx ) {
  assert_internal(n >= 0);
  return vector_alloc( (n < 0 ? 0 : n), box_null, ctx);
}

static inline box_t vector_at_int( vector_t v, integer_t n, context_t* ctx ) {
  // TODO: check bounds
  vector_at32(v,integer_clamp32(n,ctx),ctx);
}

static inline double double_abs(double d) {
  return (isfinite(d) && d < 0.0 ? -d : d);
}
 
double    random_double(context_t* ctx);
integer_t random_int(context_t* ctx);

string_t  string_host(context_t* ctx);
box_t     main_console( function_t action, context_t* ctx );
