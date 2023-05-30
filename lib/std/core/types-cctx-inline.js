/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
export function _cctx_empty() {
  return _Cctx(undefined,{obj:undefined,field_name:""})
}

export function _cctx_create(res,field_addr) {
  return _Cctx(res,field_addr);
}

export function _cctx_extend(acc,res,field_addr) {
  if (acc.res===undefined) {
    return _Cctx(res,field_addr);
  }
  else {
    acc.holeptr.obj[acc.holeptr.field_name] = res;
    return _Cctx(acc.res,field_addr);
  }
}

export function _cctx_compose(ctx1,ctx2) {
  if (ctx2.res==undefined) {
    return ctx1;
  }
  else {
    return _cctx_extend(ctx1,ctx2.res,ctx2.field_addr);
  }
}

export function _cctx_apply(acc,res) {
  if (acc.res===undefined) {
    return res;
  }
  else {
    acc.holeptr.obj[acc.holeptr.field_name] = res;
    return acc.res;
  }
}
