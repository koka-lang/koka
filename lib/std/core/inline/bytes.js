function kk_bytes_assign(b, i, new_value) {
  const fresh = new Uint8Array(b);
  fresh[i] = new_value;
  return fresh;
}