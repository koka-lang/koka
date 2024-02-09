/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

kk_decl_noinline kk_std_core_types__cctx kk_cctx_compose_shared( kk_std_core_types__cctx acc1, kk_std_core_types__cctx acc2, kk_context_t* ctx  ) {
  kk_assert_internal(!kk_cctx_is_empty(acc2));
  kk_assert_internal(!kk_block_is_unique(kk_ptr_unbox(acc2.res,ctx)));
  kk_box_t* holeptr = NULL;
  kk_box_t res = kk_cctx_copy(acc2.res,kk_field_addr_ptr(acc2.holeptr,ctx),&holeptr,ctx);
  return kk_cctx_extend(acc1,res,kk_field_addr_create(holeptr,ctx),ctx);
}