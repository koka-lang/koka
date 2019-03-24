/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
var _host = "unknown"

if (typeof window !== 'undefined' && window.document) {
  _host = "browser";
}
else if (typeof importScripts !== 'undefined') {
  _host = "webworker"
}
else if (typeof process !== undefined) {
  _host = "node"
}


/*------------------------------------------------
  Number formatting
------------------------------------------------*/

function _string_repeat(s,n) {
  if (n<=0)  return "";
  if (n===1) return s;
  if (n===2) return s+s;
  var res = "";
  while(n > 0) {
    if (n & 1) res += s;
    n >>>= 1;
    s += s;
  }
  return res;
}

function _trimzeros(s) {
  return s.replace(/\.?0+$/,"");
}

function _gformat(x,format) {
  var hex = /^[xX]([0-9]*)/.exec(format)
  if (hex) {
    var w = parseInt(hex[1]);
    var s = x.toString(16)
    if (format[0] == 'X') s = s.toUpperCase();
    return (s.length<w ? _string_repeat("0",w - s.length) + s : s );
  }
  var exp = /^[eE]([0-9]*)/.exec(format)
  if (exp) {
    var w = parseInt(exp[1]);
    return (w>0 && w<=20 ? x.toExponential(w) : x.toExponential());
  }
  var fix = /^[fF]([0-9]*)/.exec(format)
  if (fix) {
    var w = parseInt(fix[1]);
    return _trimzeros((w > 0 && w <= 20) ? x.toFixed(w) : x.toFixed(20));
  }
  var expfix = /^[gG]([0-9]*)/.exec(format)
  if (expfix) {
    var w = parseInt(expfix[1]);
    return (w>0&&w<=20 ? x.toPrecision(w) : x.toPrecision());
  }
  /* default */
  return x.toString();
}


function _double_show_exp(d,fractionDigits) {
  var s;
  if (fractionDigits < 0) {
    // use at most |fractionDigits|
    s = d.toExponential();
  }
  else {
    // use exactly |fractionDigits|.
    if (fractionDigits > 20) fractionDigits = 20;
    s = d.toExponential(fractionDigits);
  }
  return s.replace(/[eE][\+\-]?0+$/,"");
}

function _double_show_fixed(d, fractionDigits) {
  var dabs = (d < 0.0 ? -d : d);
  if (dabs < 1.0e-15 || dabs > 1.0e+21) {
    return _double_show_exp(d,fractionDigits);
  }
  if (fractionDigits < 0) {
    // use at most |fractionDigits|
    var s = d.toFixed(-fractionDigits);              // show at full precision
    var cap = /^([\-\+]?\d+)(\.\d+)$/.exec(s);
    if (!cap) return s;
    var frac = cap[2].substr(0,1 - fractionDigits);  // then cut off
    return cap[1] + frac.replace(/(?:\.|([1-9]))0+$/,"$1"); // remove trailing zeros
  }
  else {
    // use exactly fractionDigits
    if (fractionDigits > 20) fractionDigits = 20;
    return d.toFixed(fractionDigits);
  }
}

/*------------------------------------------------
  Exceptions
------------------------------------------------*/
function _exn_capture_stack(exn) {
  if ("captureStackTrace" in Error) {
    Error.captureStackTrace(exn,_InfoException);  // best on Node.js
  }
  else {
    exn.stack = (new Error()).stack; // in browsers
  }
  if (exn.stack==null) exn.stack = "";
  // strip off leaf functions from the stack trace
  exn.stack = exn.stack.replace(/\n\s*at (exn_exception|exception|(Object\.)?throw_1|Object\.error|exn_error_pattern|Object\.error_pattern|exn_error_range|Object\._vector_at)\b.*/g,"");
}

var _InfoException = (function() {
  __extends(_InfoException,Error);
  function _InfoException(message,info) {
    this.name    = (info && info._tag ? info._tag : 'std/core/exception');
    this.message = message || "unknown error";
    this.info    = info || $Error;
    _exn_capture_stack(this);
  }
  return _InfoException;
})();

// System exceptions cannot be caught but do respect finalizers
var _InfoSystemException = (function() {
  __extends(_InfoSystemException,$std_core._SystemException);
  function _InfoSystemException(message,info) {
    $std_core._SystemException.call(this,message);
    this.name    = (info && info._tag ? info._tag : 'std/core/exception');
    this.info    = info || $Error;
    _exn_capture_stack(this);
  }
  return _InfoSystemException;
})();

function exn_exception(msg,info) {
  if (info===Cancel) return new _InfoSystemException(msg,info)
                else return new _InfoException(msg,info);
}

function exn_stacktrace( exn ) {
  if (exn instanceof Error && typeof exn.stack === "string") {
    return exn.stack;
  }
  else {
    return "";
  }
}

function exn_error_pattern(loc,def) {
  throw new _InfoException( loc + (def ? ": " + def : "") + ": pattern match failure", Pattern(loc,def));
}

function _unsupported_external(msg) {
  throw new _InfoException(msg, $Error);
}

function exn_error_range() {
  throw new _InfoException( "index out of bounds", Range );
}

function exn_throw( exn ) {
  throw exn;
}


function exn_info( exn ) {
  //console.log("exn_info: " + exn.stack);
  if (exn instanceof _InfoException && exn.info != null) {
    return exn.info;
  }
  else if (exn instanceof _InfoSystemException && exn.info != null) {
    return exn.info;
  }
  else if (exn instanceof $std_core._FinalizeException) {
    return Finalize;
  }
  else if (exn instanceof RangeError) {
    return Range;
  }
  else if (exn instanceof AssertionError) {
    return Assert;
  }
  else if (exn instanceof Error && typeof exn.code === "string" ) {
    return System(exn.code);
  }
  else {
    return $Error;
  }
}

function exn_message( exn ) {
  if (exn==null) {
    return "invalid error";
  }
  if (typeof exn.get_message === "function") { // for FinalizeException
    var msg = exn.get_message();
    if (typeof msg === "string") return msg;
  }
  if (typeof exn.message === "string") {
    return exn.message;
  }
  else if (typeof exn === "string") {
    return exn;
  }
  else if (typeof exn.toString === "function") {
    return exn.toString();
  }
  else {
    return "Unknown error";
  };
}


/*------------------------------------------------
  32-bit integer operations
--------------------------------------------------*/

function _int32_multiply(x,y) {
  var xhi = (x >> 16) & 0xFFFF;
  var xlo = x & 0xFFFF;
  var yhi = (y >> 16) & 0xFFFF;
  var ylo = y & 0xFFFF;
  var hi  = ((xhi * ylo) + (xlo * yhi));
  return (((hi << 16) + (xlo * ylo))|0)
}

function _int32_cmod(x,y) {
  if (y === 0) throw "modulus of zero";
  return ((x%y)|0);
}

function _int32_cdiv(x,y) {
  if (y === 0) throw "division by zero";
  return ((x/y)|0);
}

/*------------------------------------------------
  list helpers
------------------------------------------------*/

// Create a list with from a vector in constant stack space
function _vlist(elems,tail) {
  var xs = tail || Nil;
  if (elems!=null && elems.length>0) {
    for(var i = elems.length - 1; i >= 0; i--) {
      var elem = elems[i];
      if (elem !== undefined) xs = Cons(elem,xs);
    }
  }
  return xs;
}

// Create an array from a list with constant stack space
function _unvlist(list) {
  var elems = [];
  while(list) {
    elems.push(list.head);
    list = list.tail;
  }
  return elems;
}

// Create a vector with a function initializer
function _vector(n, f) {
  if (n<=0) return [];
  var a = new Array(n);
  for(var i = 0; i < n; i++) {
    a[i] = f(i);
  }
  return a;
}

// Index a vector
function _vector_at( v, i ) {
  var j = _int_to_int32(i);
  var x = v[j];
  if (x === undefined) { exn_error_range(); }
  return x;
}


/*------------------------------------------------
  General javascript helpers
------------------------------------------------*/
// make a shallow copy
function _copy(obj) {
  if (typeof obj !== 'object') return obj;
  var value = obj.valueOf();
  if (obj != value) return new obj.constructor(value);
  var newobj = {};
  for( var prop in obj) newobj[prop] = obj[prop];
  return newobj;
}

// get the fields of an object
function _fields(obj) {
  var props = [];
  for (var prop in obj) props.push(prop);
  return props;
}

// Export module `mod` extended with `exp`. Modifies `exp` in place and assigns to mod
function _export(mod,exp) {
  for(var prop in mod) {
    if (exp[prop] === undefined) {
      exp[prop] = mod[prop];
    }
  }
  return exp;
}

/* assign here so inlined primitives are available in system.core itself */
$std_core = _export($std_core, {
            "_export": _export
            // primitive operations emitted by the compiler
            , "_int32_multiply": _int32_multiply
            , "_int32_cmod": _int32_cmod
            , "_int32_cdiv": _int32_cdiv
            , "vlist": _vlist
            , "_vector_at": _vector_at
            , "_unsupported_external": _unsupported_external
            // integer operations that will be inlined
            , "_int_string": _int_string
            , "_int_double": _int_double
            , "_int_to_int32": _int_to_int32
            , "_double_to_int32": _double_to_int32
            ,"_double_round": _double_round
            , "_int_to_double": _int_to_double
            , "_int_iszero": _int_iszero
            , "_int_isodd": _int_isodd
            , "_int_negate": _int_negate
            , "_int_abs": _int_abs
            , "_int_sign": _int_sign
            , "_int_add": _int_add
            , "_int_sub": _int_sub
            , "_int_mul": _int_mul
            , "_int_div": _int_div
            , "_int_mod": _int_mod
            , "_int_divmod": _int_divmod
            , "_int_compare": _int_compare
            , "_int_eq": _int_eq
            , "_int_ne": _int_ne
            , "_int_gt": _int_gt
            , "_int_ge": _int_ge
            , "_int_lt": _int_lt
            , "_int_le": _int_le
            });


/*------------------------------------------------
  double arithmetic
------------------------------------------------*/


var _double_trunc = Math.trunc || function(x){ return x - x%1; };


/*------------------------------------------------
  integer arithmetic
------------------------------------------------*/


// We represent integers as a regular number as long as it is within _min_precise and _max_precise.
// Outside that we use bigInt objects.
const _max_precise = 9007199254740991; // 2^53 -1
const _min_precise = -_max_precise;

const _max_int32 =  0x7FFFFFFF;
const _min_int32 = -0x80000000;

// is a number small?
function _is_small(x) {
  return (x >= _min_precise && x <= _max_precise);
}

// If a big integer becomes small again, convert to a number
function _unbig(x) {
  if (x.isSmall) {
    if (_is_small(x.value)) return x.value;
  }
  else if (x.value.length===1) {
    var v = x.value[0];
    if (x.sign) v = -v;
    if (_is_small(v)) return v;
  }
  return x;
}

// Round a double with rounding to even on a tie.
function _double_round(d) {
    var n = Math.round(d); // rounds to +Infinity on a tie
    return (n - d == 0.5 && n % 2 != 0 ? n - 1 : n);  // if it was a tie, and n is odd, decrement by 1
}

// create an int from a double.
function _int_double(x) {
  if (_is_small(x)) return _double_round(x);
  if (isFinite(x)) return bigInt(x);
  if (x===Infinity) return _max_int32;
  if (x===-Infinity) return _min_int32;
  return 0;
}

// create an int from a string.
function _int_string(s) {
  return _unbig(bigInt(s));
}


// Clamp a big integer into a 32 bit integer range.
function _int_to_int32(x) {
  const v = (_is_small(x) ? x : x.toJSNumber());
  if (v > _max_int32) return _max_int32;
  if (v < _min_int32) return _min_int32;
  return (v|0);
}

// Clamp a double into a 32 bit integer range.
function _double_to_int32(x) {
  if (x > _max_int32) return _max_int32;
  if (x < _min_int32) return _min_int32;
  if (isNaN(x)) return 0;
  return (x|0);
}


function _int_to_double(x) {
  return (_is_small(x) ? x : x.toJSNumber());
}


// Wrappers for basic integer operations.
// Generally we first perform the operation and check afterwards for "overflow",
// and back down to bigInt operations only when needed.

function _big_add(x,y)    { return _unbig(bigInt(x).add(bigInt(y))); }
function _big_sub(x,y)    { return _unbig(bigInt(x).subtract(bigInt(y))); }
function _big_mul(x,y)    { return _unbig(bigInt(x).multiply(bigInt(y))); }
function _big_cdivmod(x,y){ return bigInt(x).divmod(bigInt(y)); }
function _big_cdiv(x,y)   { return _unbig(bigInt(x).divide(bigInt(y))); }
function _big_cmod(x,y)   { return _unbig(bigInt(x).mod(bigInt(y))); }
function _big_compare(x,y){ return bigInt(x).compare(bigInt(y)); }
function _big_negate(x)   { return bigInt(x).negate(); }
function _big_inc(x)      { return _unbig(bigInt(x).next()); }
function _big_dec(x)      { return _unbig(bigInt(x).prev()); }

function _big_count_pow10(x)  { return bigInt(x).count_pow10(); }
function _big_count_digits(x) { return bigInt(x).count_digits(); }
function _big_div_pow10(x,n)  { return _unbig(bigInt(x).div_pow10(n)); }
function _big_mul_pow10(x,n)  { return _unbig(bigInt(x).mul_pow10(n)); }

function _big_pow(x,n) { return _unbig(bigInt(x).pow(n)); }

function _big_divmod(x,y) {
  const d  = bigInt(y);
  const qr = _big_cdivmod(x,d);
  var q = qr.quotient;
  var r = qr.remainder;
  if (r.isNegative()) {
    if (d.isPositive()) { q = q.prev(); r = r.add(d) }
                   else { q = q.next(); r = r.subtract(d) }
  }
  return _tuple2_(_unbig(q),_unbig(r));
}

function _int_negate(x) {
  const z = -x;
  return (_is_small(z) ? z : _big_negate(x));
}

function _int_iszero(x) {
  return (!isNaN(x) ? x===0 : x.isZero());
}

function _int_isodd(x) {
  return (!isNaN(x) ? (x&1)===1 : x.isOdd());
}


function _int_abs(x) {
  return (!isNaN(x) ? Math.abs(x) : x.abs());
}

function _int_add(x,y) {
  const z = x + y;
  return (_is_small(z) ? z : _big_add(x,y));
}

function _int_sub(x,y) {
  const z = x - y;
  return (_is_small(z) ? z : _big_sub(x,y));
}

function _int_mul(x,y) {
  const z = x * y;
  return (_is_small(z) ? z : _big_mul(x,y));
}

function _int_cdivmod(x,y) {
  const q = _double_trunc(x / y);
  if (!isNaN(q)) {
    return _tuple2_(q,(x%y));
  }
  else {
    const qr = _big_cdivmod(x,y);
    return _tuple2_(_unbig(qr.quotient),_unbig(qr.remainder));
  }
}

function _int_cdiv(x,y) {
  const q = _double_trunc(x / y);
  return (!isNaN(q) ? q : _big_cdiv(x,y));
}

function _int_cmod(x,y) {
  const r = (x % y);
  return (!isNaN(r) ? (r) : _big_cmod(x,y));
}

function _int_divmod(x,y) {
  if (_int_iszero(y)) return 0;
  var q = _double_trunc(x / y);
  if (!isNaN(q)) {
    var r = x%y;
    if (r<0) {
      if (y>0) { q = q - 1; r = r + y; }
          else { q = q + 1; r = r - y; }
    }
    return _tuple2_(q,r);
  }
  else return _big_divmod(x,y)
}

function _int_div(x,y) {
  if (_int_iszero(y)) return 0;
  const q = _double_trunc(x/y);
  if (!isNaN(q)) {
    const r = (x%y);
    return (r<0 ? (y>0 ? q-1 : q+1) : q);
  }
  else return _big_divmod(x,y).fst;
}

function _int_mod(x,y) {
  if (_int_iszero(y)) return 0;
  const r = (x%y);
  if (!isNaN(r)) {
    return (r<0 ? (y>0 ? r+y : r-y) : r);
  }
  else return _big_divmod(x,y).snd;
}


function _int_compare(x,y) {
  const d = x - y;
  if (!isNaN(d)) {
    return (d>0 ? Gt : (d<0 ? Lt : Eq));
  }
  else {
    const c = _big_compare(x,y);
    return (c>0 ? Gt : (c<0 ? Lt : Eq));
  }
}

function _int_sign(x) {
  if (!isNaN(x)) {
    return (x>0 ? Gt : (x<0 ? Lt : Eq));
  }
  else {
    return (x.isZero() ? Eq : (x.isPositive() ? Gt : Lt));
  }
}

function _int_eq(x,y)   { return _int_compare(x,y)===Eq; }
function _int_ne(x,y)   { return _int_compare(x,y)!==Eq; }
function _int_lt(x,y)   { return _int_compare(x,y)===Lt; }
function _int_le(x,y)   { return _int_compare(x,y)!==Gt; }
function _int_gt(x,y)   { return _int_compare(x,y)===Gt; }
function _int_ge(x,y)   { return _int_compare(x,y)!==Lt; }


function _int_pow(i,exp) {
	if (_is_small(i)) {
		var j = Math.pow(i);
		if (_is_small(j)) return j;
	}
	return _big_pow(i,exp);
}

function _int_count_pow10(i) {
  return (_is_small(i) ? bigInt._count_pow10(i) : _big_count_pow10(i) );
}

function _int_count_digits(i) {
  return (_is_small(i) ? bigInt._count_digits(i) : _big_count_digits(i) );
}

function _int_mul_pow10(i,n) {
  return (_is_small(i) && n <= 14 ? _int_mul(i,Math.pow(10,n)) : _big_mul_pow10(i,n) );
}

function _int_cdiv_pow10(i,n) {
  return (_is_small(i) && n <= 14 ? _int_cdiv(i,Math.pow(10,n)) : _big_div_pow10(i,n) );
}

function _int_showhex(x,upper) {
  const s = x.toString(16);
  return (upper ? s.toUpperCase() : s);
}

function _int_parse(s,hex) {
  if (s==="") return Nothing;
  const cappre  = /^([\-\+])?(0[xX])?(.*)$/.exec(s);
  const sdigits = cappre[3].toLowerCase();
  const sign    = cappre[1] || "";
  if (cappre[2]) hex = true;
  const rx = (hex ? /^[0-9a-f]+$/ :
                     /^[0-9]+(?:e\+?[0-9]+)?$/);
  const cap = rx.exec(sdigits);
  if (!cap) return Nothing;
  else if (hex) return Just(_unbig(bigInt(sign + sdigits,16)));
  else return Just(bigInt(sign + sdigits));
}
