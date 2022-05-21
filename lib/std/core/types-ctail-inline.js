/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
export function _ctail_unit() {
  return _CTail(undefined,{value:undefined,field:""})
}

export function _ctail_compose(acc,res,field) {
  if (acc.res===undefined) {
    return _CTail(res,field);
  }
  else {
    acc.hole.value[acc.hole.field] = res;
    return _CTail(acc.res,field);
  }
}

export function _ctail_apply(acc,res) {
  if (acc.res===undefined) {
    return res;
  }
  else {
    acc.hole.value[acc.hole.field] = res;
    return acc.res;
  }
}
