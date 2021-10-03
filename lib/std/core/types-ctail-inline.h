





/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline kk_box_t kk_ctail_hole(void) {
  return kk_int_box(0);
}

static inline kk_std_core_types__ctail kk_ctail_nil(void) {
  return kk_std_core_types__new_CTail( kk_ctail_hole(), NULL, NULL );
}

static inline kk_std_core_types__ctail kk_ctail_link( kk_std_core_types__ctail acc, kk_box_t res, kk_box_t* field ) {
  return kk_std_core_types__new_CTail( (kk_likely(acc.hole != NULL) ? (*(acc.hole) = res, acc.res) : res ), field, NULL );
}

static inline kk_box_t kk_ctail_resolve( kk_std_core_types__ctail acc, kk_box_t res ) {
  return (kk_likely(acc.hole != NULL) ? (*(acc.hole) = res, acc.res) : res );
}

