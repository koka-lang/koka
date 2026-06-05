function kk_string_from_bytes(bs) {
  if (typeof TextDecoder !== "undefined") {
    //console.trace( "bytes: " + typeof bs );
    return (new TextDecoder("utf-8",{fatal:false,ignoreBOM:false})).decode(bs);
  }
  else if (typeof Buffer !== "undefined") {
    return Buffer.from(bs).toString();
  }
  else {
    // todo: improve the fallback to correctly decode utf-8?
    return String.fromCharCode.apply(null,b);
  }
}

function kk_string_to_bytes(s) {
  if (typeof TextEncoder !== "undefined") {
    return (new TextEncoder("utf-8",{fatal:false,ignoreBOM:false})).encode(s);
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
  bss.forEach( (bs) => total_len += bs.length );
  var bsnew = new Uint8Array(total_len);
  var offset = 0;
  bss.forEach( (bs) => {
    bsnew.set(bs,offset);
    offset += bs.length;
  });
  return bsnew;
}

function kk_bytes_assign(b, i, new_value) {
  const fresh = new Uint8Array(b);
  fresh[i] = new_value;
  return fresh;
}


function kk_bytes_substring(b, start, len) {
  if (len == 0 || start >= b.length) return [];  
  if (start < 0) start = 0;
  if (len < 0 || start + len > b.length) { len = b.length - start; }
  return kk_string_from_bytes(b.subarray(start,start + len));
}

function kk_bytes_subvector(b, start, len) {
  if (len == 0 || start >= b.length) return [];  
  if (start < 0) start = 0;
  if (len < 0 || start + len > b.length) { len = b.length - start; }
  return Array.from(b.subarray(start,start + len), b => (b <= 127 ? b : b - 256))   // to signed int8
}

function kk_bytes_from_vector(v) {
  return Uint8Array.from( v, b => (b >= 0 ? b : 256 + b) )  // from signed int8
}

// count up to 3 initial continuation bytes
function kk_bytes_utf8_partial_pre(b) {
  var i = 0;
  for( ; i < b.length && i < 3; i++ ) {
    if ((b[i] & 0xC0) !== 0x80) break;
  }
  return i;
}

// count up to 3 bytes at the end for an unfinished utf-8 code point
function kk_bytes_utf8_partial_post(b) {
  const len = b.length
  var i = 1
  for( ; i <= 3 && i <= len; i++) {
    var b = b[len - i]
    if ((b[i] & 0xC0) !== 0x80) {
      // start of utf8 sequence
      if ((b & 0xF8) === 0xF0) {  // 4 byte sequence && i <= 3
        return i;
      }
      else if (i<=2 && ((b & 0xF0) === 0xE0)) {  // 3 byte sequence && i <= 2
        return i;
      }
      else if (i<=1 && ((b & 0xD0) === 0xC0)) {  // 2 byte sequence && i == 1
        return i;
      }
      else {
        return 0;  // full sequence or invalid utf8
      }
    }
  }
  return 0; // all continuation bytes (caught by _pre)
}
