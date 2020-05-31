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
    return (i==box_null ? __std_core_types__new_Nothing(ctx) : __std_core_types__new_Just(box_integer_t(i),ctx));
  }
}

datatype_t string_to_list(string_t s, context_t* ctx);
string_t   string_from_list(datatype_t cs, context_t* ctx);
