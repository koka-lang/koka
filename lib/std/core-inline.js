/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
var $std_core;

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

var _gformat = function(x,format) {
  var hex = /^[xX]([0-9]*)/.exec(format)
  if (hex) {
    var s = x.toString(16)
    if (format[0] == 'X') s = s.toUpperCase();
    return pad_left( s, hex[1], "0" )
  }
  var exp = /^[eE]([0-9]*)/.exec(format)
  if (exp) {
    return (exp[1] > 0 && exp[1] < 22 ? x.toExponential(exp[1]) : x.toExponential());
  }
  var fix = /^[fF]([0-9]*)/.exec(format)
  if (fix) {
    return (fix[1] > 0 && fix[1] < 22 ? x.toFixed(fix[1]) : x.toFixed());
  }
  var expfix = /^[gG]([0-9]*)/.exec(format)
  if (expfix) {
    return (expfix[1] > 0 && expfix[1] < 22 ? x.toPrecision(expfix[1]) : x.toPrecision());
  }
  /* default */
  return x.toString();  
}


/*------------------------------------------------
  Exceptions
------------------------------------------------*/
function _error(s) {
  throw s;
}

function _primcatch(action,handler,_k) {
  if (_k!==undefined) {
    // if in cps mode, use an effect handler
    return hcatch(action,handler,_k);
  }
  else {
    // otherwise we can catch normally
    try {
      return action();
    }
    catch(exn) {
      return handler(exn)
    }
  }
}

function _primfinally(action,handler,_k) {
  if (_k !== undefined) {
    // if in cps, use an effect handler
    return hfinally(action,handler,_k);
  }
  else {
    try {
      return action();
    }
    finally {
      handler();
    }
  }
}

function _error_pattern_match(location,definition) { 
  throw (location + (definition ? (": " + definition) : "") + ": pattern match failure"); 
};

/*------------------------------------------------
  32-bit integer operations
--------------------------------------------------*/

var _int32_multiply = function(x,y) {
  var xhi = (x >> 16) & 0xFFFF;
  var xlo = x & 0xFFFF;
  var yhi = (y >> 16) & 0xFFFF;
  var ylo = y & 0xFFFF;
  var hi  = ((xhi * ylo) + (xlo * yhi));
  return (((hi << 16) + (xlo * ylo))|0)
}

var _int32_cmod = function(x,y) {
  if (y === 0) throw "modulus of zero";
  return ((x%y)|0);
}

var _int32_cdiv = function(x,y) {
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
      xs = Cons(elem,xs);
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

/* assign here so inlined primitives are available in system.core itself */
$std_core = { "int_multiply": _int32_multiply
            , "int_cmod": _int32_cmod
            , "int_cdiv": _int32_cdiv
            , "vlist": _vlist
            , "Yield": Yield
            , "_intconst": _intconst
            }



/*------------------------------------------------
  Big integer arithmetic
------------------------------------------------*/

// We represent integers as a regular number as long as it is within _min_exact and _max_exact.
// Outside that we use bigInt objects.
const _max_exact = 67108864;    // = 2^26 since 2^52 is still precise.
const _min_exact = -_max_exact;

// If a big integer becomes small again, convert to a number
function _unbig(x) {
  if (x.isSmall) {
    if (x.value>_min_exact && x.value<_max_exact) return x.value;
  }
  else if (x.value.length===1) {
    const v = x.value[0];
    if (v>_min_exact && v<_max_exact) return v;
  }
  return x;
}

// create an int
function _int_small(x) {
  return (x>_min_exact && x<_max_exact ? x : _bigInt(x));
}

function _int_const(s) {
  return _unbig(_bigInt(s));
}

// Wrappers for basic integer operations. 
// Generally we first perform the operation and check afterwards for "overflow",
// and back down to bigInt operations only when needed.

function _big_add(x,y) { return _unbig(_bigInt(x).add(_bigInt(y))); }
function _big_sub(x,y) { return _unbig(_bigInt(x).subtract(_bigInt(y))); }
function _big_mul(x,y) { return _unbig(_bigInt(x).multiply(_bigInt(y))); }
function _big_divmod(x,y) { return _unbig(_bigInt(x).divmod(_bigInt(y))); }
function _big_div(x,y) { return _unbig(_bigInt(x).divide(_bigInt(y))); }
function _big_mod(x,y) { return _unbig(_bigInt(x).mod(_bigInt(y))); }
function _big_compare(x,y) { return _bigInt(x).compare(_bigInt(y)); }

function _int_add(x,y) {
  const z = x + y;  
  return (z>_min_exact && z<_max_exact ? z : _big_add(x,y));
}

function _int_sub(x,y) {
  const z = x - y;  
  return (z>_min_exact && z<_max_exact ? z : _big_sub(x,y));
}

function _int_mul(x,y) {
  const z = x * y;  
  return (z>_min_exact && z<_max_exact ? z : _big_mul(x,y));
}

function _int_cdivmod(x,y) {
  const q = (x / y);  
  if (!isNaN(q)) {
    return _tuple2_((q|0),(x%y));
  }
  else {
    const qr = _big_divmod(x,y);
    return _tuple2_(qr.quotient,qr.remainder);
  }
}

function _int_cdiv(x,y) {
  const q = (x / y);  
  return (!isNaN(q) ? (q|0) : _big_div(x,y));
}

function _int_cmod(x,y) {
  const r = (x % y);  
  return (!isNaN(r) ? (r) : _big_mod(x,y));
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

function _int_odd(x) {
  const b = x&1;
  return (b===1||b===0 ? b===1 : x.isOdd());
}

function _int_double(x) {
  return (typeof x === "number" ? x : x.toJSNumber());
}

const _max_int32 =  0x7FFFFFFF;
const _min_int32 = -0x80000000;

function _int_clamp32(x) {
  if (x>_min_exact&&x<_max_exact) return x;
  const v = x.toJSNumber();
  if (v>_max_int32 || v===Infinity) return _max_int32;
  if (v<_min_int32 || v===-Infinity) return _min_int32;
  return v;
}