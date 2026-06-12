export function kk_lazy_atomic_enter(x,indirect_tag) {
  if (x._kk_lazy_blocked) {
    // todo: throw for recursive enter?
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

function kk_indirect_compress(root, indirect_tag) {
  // assert: root._tag == indirect_tag
  // walk the indirections until we find the final value `val`
  var val = null;
  var b = root;
  while (b && b._tag == indirect_tag) {
    b = val = b._indirect;
  }
  // and again, updating all indirections with `val`
  b = root;
  while (b && b._tag == indirect_tag) {
    const next = b._indirect;
    Object.assign(b,val);
    b = next;
  }
  return val;
}
