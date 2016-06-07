/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
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
  Formatting
------------------------------------------------*/
var _padleft = function(str,n,pad) { 
  if (n == undefined) return str; 
  n = n - str.length;
  if (n > 0) {
    if (pad === undefined) pad = " ";
    return (Array(n+1).join(pad) + str);
  }
  return str;
}

var _gformat = function(x,format) {
  var hex = /^[xX]([0-9]*)/.exec(format)
  if (hex) {
    var s = x.toString(16)
    if (format[0] == 'X') s = s.toUpperCase();
    return _padleft( s, hex[1], "0" )
  }
  var exp = /^[eE]([0-9]*)/.exec(format)
  if (exp) {
    return (exp[1] > 0 ? x.toExponential(exp[1]) : x.toExponential());
  }
  var fix = /^[fF]([0-9]*)/.exec(format)
  if (fix) {
    return (fix[1] > 0 ? x.toFixed(fix[1]) : x.toFixed());
  }
  var expfix = /^[gG]([0-9]*)/.exec(format)
  if (expfix) {
    return (expfix[1] > 0 ? x.toPrecision(expfix[1]) : x.toPrecision());
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
  Various
------------------------------------------------*/

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

// Create a list with constant stack space
function _list(elems,tail) {
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
function _unlist(list) {
  var elems = [];
  while(list) {
    elems.push(list.head);
    list = list.tail;
  }
  return elems;
}

/*------------------------------------------------
  String codepoints
------------------------------------------------*/

var _uni_replace = 0xFFFD;

function _cp_at( s, idx, f ) {
  if (typeof s !== "string") return _uni_replace;
  if (idx==null || idx >= s.length) idx = s.length - 1;  
  var i = 0;
  var cpi = 0;
  var code = _uni_replace;
  while( i <= idx) {
    var i0 = i;
    var code = s.charCodeAt(i);
    i++;
    if (code >= 0xD800 && code <= 0xDBFF) {  // surrogate pair?
      if (i >= s.length) // only half of the surrogate?
        code = _uni_replace;
      else {
        var hi = s.charCodeAt(i);
        i++;
        if (hi < 0xDC00 || hi > 0xDFFF)  // invalid surrogate?
          code = _uni_replace;
        else
          code = (code - 0xD800) * 0x400 + (hi - 0xDC00) + 0x10000;      
      }
    }
    if (f) {
      if (f(code,cpi,i0)) break;
    }
    cpi++;
  }
  return code;
}

function _cp_list( s ) {
  var codes = [];
  _cp_at( s, s.length, function(code,ofs) { 
    codes.push(code); 
    return false; 
  });
  return _list(codes);
}

function _cp_unlist( list ) {
  var ucs2 = [];
  var s = "";
  while(list) {
    var code = list.head;
    list = list.tail;
    if (code < 0xFFFF) {
      ucs2.push(code);
    } 
    else if (code > 0x10FFFF) {
      ucs2.push(_uni_replace);
    }
    else {
      code = code - 0x10000;
      ucs2.push( (code >> 10) + 0xD800 );
      ucs2.push( (code % 0x0400) + 0xDC00 );
    }
    if (ucs2.length > 1024) {  // chunk it up for long strings
      s += String.fromCharCode.apply(null,ucs2);
      ucs2 = [];
    }
  }
  s += String.fromCharCode.apply(null,ucs2);
  return s;
}

function _cp_index( s, i ) {
  if (i <= 0) return i;
  var len = 0;
  var cpi = -1;
  _cp_at( s, s.length, function(code,_cpi,_i) { 
    len++;
    if(_i===i) {
      cpi = _cpi;
      return true;
    }
    else return false;
  });
  return (cpi < 0 ? len : cpi);
}

function _ucs2_index( s , cpi ) {
  if (cpi <= 0) return cpi;
  var i = s.length;
  _cp_at( s, s.length, function(code,_cpi,_i) { 
    if (_cpi===cpi) {
      i = _i;
      return true;
    }
    else return false;
  });
  return i;
}

function _cp_length( s ) {
  if (typeof s !== "string") return 0;
  var len = 0;
  _cp_at( s, s.length, function(code,cpi,i) { 
    len++; 
    return false; 
  });
  return len;
}

function _cp_substr( s, start, len ) {
  var _start = -1;
  var _len   = undefined;
  _cp_at( s, s.length, function(code,cpi,i) {
    if (cpi < start) {
      return false;
    }
    else {
      if (cpi === start) _start = i;
      if (len <= 0) {
        _len = (i - _start);
        return true; // break
      }
      else {
        len--;
        return false;
      }
    }
  });
  return s.substr(_start,_len);
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
            , "conslist": _list
            , "Yield": Yield
            }

