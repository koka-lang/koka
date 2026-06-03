function kk_string_from_bytes(bs) {
  if (typeof TextEncoder !== "undefined") {
    return new TextEncoder("utf-8",{fatal:false,ignoreBOM:false}).decode(bs);
  }
  else if (typeof Buffer !== "undefined") {
    return Buffer.from(bs).toString()
  }
  else {
    // todo: improve the fallback?
    return String.fromCharCode.apply(null,b);
  }
}

function kk_string_to_bytes(s) {
  if (typeof TextEncoder !== "undefined") {
    return new TextEncoder("utf-8",{fatal:false,ignoreBOM:false}).encode(s);
  }
  else if (typeof Buffer !== "undefined") {
    return Buffer.from(s,"utf-8");  // buffer is a subclass of UInt8Array
  }
  else {
    // todo: improve the fallback?
    return Uint8Array.from(text.split('').map(b => b.charCodeAt(0)));
  }
}

function kk_bytes_adjust_length(bs,newlen) {
  if (newlen == bs.length) {
    return bs;
  }
  else if (newlen < bs.length) {
    return bs.slice(0,newlen);
  }
  else {
    const bsnew = new Uint8Array(newlen).fill(0);
    bsnew.set(bs,0);
    return bsnew;
  }
}

function kk_bytes_join(bss) {
  var total_len = 0;
  bss.foreach( (bs) => total_len += bs.length );
  var bsnew = new Uint8Array(total_len);
  var offset = 0;
  bss.foreach( (bs) => {
    bsnew.set(bs,offset);
    offset += bs.length;
  });
  return bss;
}

function kk_bytes_assign(b, i, new_value) {
  const fresh = new Uint8Array(b);
  fresh[i] = new_value;
  return fresh;
}