






/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// static inline kk_std_core_types__order kk_int_as_order(int i,kk_context_t* ctx) {
//   return (i==0 ? kk_std_core_types__new_Eq(ctx) : (i > 0 ? kk_std_core_types__new_Gt(ctx) : kk_std_core_types__new_Lt(ctx)));
// }

static inline kk_std_core_types__maybe kk_integer_xparse( kk_string_t s, bool hex, kk_context_t* ctx ) {
  kk_integer_t i;
  bool ok = (hex ? kk_integer_hex_parse(kk_string_cbuf_borrow(s,NULL,ctx),&i,ctx) : kk_integer_parse(kk_string_cbuf_borrow(s,NULL,ctx),&i,ctx) );
  kk_string_drop(s,ctx);
  return (ok ? kk_std_core_types__new_Just(kk_integer_box(i,ctx),ctx) : kk_std_core_types__new_Nothing(ctx));
}

static inline kk_std_core_types__tuple2 kk_integer_div_mod_tuple(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_integer_t mod;
  kk_integer_t div = kk_integer_div_mod(x,y,&mod,ctx);
  return kk_std_core_types__new_Tuple2(kk_integer_box(div,ctx),kk_integer_box(mod,ctx),ctx);
}
