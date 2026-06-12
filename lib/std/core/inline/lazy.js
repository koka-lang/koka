export function kk_lazy_atomic_enter(x,indirect_tag) {
  if (x._kk_lazy_blocked) {
    return false;
  }
  else {
    x._kk_lazy_blocked = true;
    return true;
  }
}

export function kk_lazy_atomic_leave(x) {
  x._kk_lazy_blocked = false;
}

export function kk_lazy_memoize(target, x) {
  // todo: should do this statically during codegen (like in C ParcReuse)
  return Object.assign(target,x);
}
