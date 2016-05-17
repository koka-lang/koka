/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
var $std_core;

var $inBrowser = (typeof window !== 'undefined' && window.document);
var $inWebWorker = (!$inBrowser && typeof importScripts !== 'undefined');

/*------------------------------------------------
  async interface
------------------------------------------------*/
function async_create() {
  return { done: false, value: null }; // & exn, on, on-exn
}

function async_create_done(x) {
  return { done: true, value: x };
}

function async_is_done(async) {
  return (async.done ? true : false);
}

function async_on(async,f) {
  var result = async_create();
  if (async.done) {
    if (!async.exn) {
      async_supply(result,f(async.value));
    }
  }
  else {    
    if (async.on) {
      var g = async.on;
      async.on = function(x) { g(x); async_supply(result,f(x)); };    
    }
    else {
      async.on = function(x) { async_supply(result,f(x)); };
    }
  }
  return result;
}


function async_on_exn(async,f) {
  var result = async_create();
  if (async.done) {
    if (async.exn) {
      async_supply(result,f(async.exn));
    }
  }
  else {    
    if (async.on_exn) {
      var g = async.on_exn;
      async.on_exn = function(x) { g(x); async_supply(result,f(x)); };    
    }
    else {
      async.on_exn = function(x) { async_supply(result,f(x)); };
    }
  }
  return result;
}

function async_supply(async,x) {
  if (async.done) return;
  async.done = true;
  async.value = x;
  if (async.on) { 
    async.on(x); 
    async.on = null; async.on_exn = null;
  }
}

function async_supply_exn(async,exn) {
  if (async.done) return;
  async.done = true;
  async.exn = exn;
  if (async.on_exn) { 
    async.on_exn(exn); 
    async.on = null; async.on_exn = null;
  }
}

/*------------------------------------------------
  Console for Node
------------------------------------------------*/
var $node = (function() {
  if (typeof window !== "undefined" || typeof process === "undefined") {
    // probably on the browser
    return {
      in: function() { return null; },
      out: function() { return null; }
    }
  }

  // on Node
  var nodeout = { print: function(s) { process.stdout.write(s); } };
  var nodein = null;

  function get_nodein() {
    if (!nodein) { 
      var readline = require("readline");
      nodein = readline.createInterface( { input: process.stdin /*, output: process.stdout */ });
      nodein.print = nodeout.print;
      nodein.on('line', function(s) {
        //console.log("line event");
        if (nodein.onreadline) {
          var async = nodein.onreadline;
          nodein.onreadline = null;
          async_supply(async,s);
        }
        if (nodein.onreadline==null) {
          nodein.close(); // no more readlines -> close the app
          nodein = null;
        }
      });
    }
    return nodein;
  }

  return {
    in: get_nodein,
    out: function() { return nodeout; },
  };
})();


/*------------------------------------------------
  Console for Browser
------------------------------------------------*/
var $browser = (function(){
  if (typeof window === "undefined" || !window.document) {
    return {
      in: function() { return null; },
      out: function() { return null; }
    }
  }

  var escapes = {
      '&': '&amp;', // & first!
      '<': '&lt;',
      '>': '&gt;',
      '\'': '&apos;',
      '"': '&quot;',
      '\n': '<br>',
      '\r': '',
  };
  var escapes_regex = new RegExp("[" + Object.keys(escapes).join("") + "]", "g");

  function html_escape(txt) {
    return txt.replace(escapes_regex, function (s) {
      var r = escapes[s];
      return (r ? r : "");
    });
  }

  function get_console() {
    var cons = document.getElementById("koka-console");
    if (cons==null) {
      cons = document.createElement("div");
      cons.id = "koka-console";
      cons.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
      cons.style.fontSize = "12pt";
      cons.style.width = "99%";
      document.body.appendChild(cons);
    }
    if (cons.display == "none") return null;
    return cons;
  }

  function get_console_out() 
  {
    var output = document.getElementById("koka-console-out");
    if (!output) {
      var cons = get_console();
      if (!cons) return null;

      output = document.createElement("div");
      output.id = "koka-console-out";
      output.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
      output.style.fontSize = "12pt";
      output.style.width = "99%";
      output.style.height = "30ex";
      output.style.border = "gray solid 1px";
      output.wrap="off";
      output.style.overflow = "auto";
      output.style.whiteSpace = "pre";
      output.style.padding = "2px";
      output.style.margin = "2px";
      output.style.paddingBottom = "4px";
      output.readOnly = true;
      cons.appendChild(output);
    }

    if (!output.print) {
      output.print_html = function(s) {
        output.innerHTML = output.innerHTML + s;
        
        // try to scroll to the end
        if (output.createTextRange) {
          output.createTextRange().scrollIntoView(false);
        }
        else if (output.scrollTop !== undefined) {
          output.scrollTop = output.scrollHeight;
        }    
      };
      output.print = function(s) {
        output.print_html(html_escape(s));
      }
    }
    return output;
  }

  function caret_to_end(elem) {
    if (!elem || !elem.value || !elem.value.length) return;
    var pos = elem.value.length;
    if (pos===0) return;

    if (elem.createTextRange) { /* ie */
      var rng = elem.createTextRange();
      rng.collapse(true);
      rng.moveEnd("character",pos);
      rng.moveStart("character",pos);
      rng.select();
    }
    else if (elem.setSelectionRange) {  /* the rest */
      elem.setSelectionRange(pos,pos);
    }
  }

  function overlap(s,t) {
    if (!s || !t) return 0;
    var len = max(s.length,t.length);
    var i = 0;
    while(i < len) {
      if (s[i] !== t[i]) return i;
      i++;
    }
    return i;
  }

  var console_input_init = false;
  function get_console_input() 
  {
    var input = document.getElementById("koka-console-in");
    if (!input) {

      var cons = get_console();
      if (!cons) return null;

      get_console_out(); // ensure there is an output pane

      input = document.createElement("input");
      input.type = "text";
      input.id = "koka-console-in";
      input.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
      input.style.fontSize = "12pt";
      input.style.width = "99%";
      input.style.border = "gray solid 1px";
      input.style.padding = "2px";
      input.style.margin = "2px";
      cons.appendChild(input);
    }
    
    if (!console_input_init) {
      console_input_init = true;
      var prompt = "> ";
      input.value = prompt;
      input.onfocus = function() {
        caret_to_end(input); /* needed on IE 10 */
      }
      input.onkeypress = function(ev) {
        ev = ev || window.event;
        if(ev.keyCode == 13) { 
          var content = input.value;
          input.value = prompt;
          var i = overlap(prompt,content);            // remove prompt prefix (if present)
          if (i > 0) content = content.substring(i);
          print_html("<span style='color: gray'>" + html_escape(prompt + content) + "</span><br>")
          if (input.onreadline) {
            var async = input.onreadline;
            input.onreadline = null;
            async_supply(async,content);
          }
        }
      };
    }

    return input;
  }
    
  function print_html(msg) {
    var output = get_console_out();
    if (output && output.print_html) {
      output.print_html(msg);
    }
  }

  return {
    in: get_console_input,
    out: get_console_out,
  }
})();

/*------------------------------------------------
  Console 
------------------------------------------------*/
function $print(msg) {
  var output = $node.out() || $browser.out();
  if (output && output.print) {
    output.print(msg);
  }
}

function $println(msg) {
  $print(msg + "\n");
}

function $trace(msg) {
  if (typeof console !== "undefined") {
    console.log(msg)
  }
}

function $read_line() 
{
  var async;
  var input = $node.in() || $browser.in();
  if (input) {
    if (!input.onreadline) input.onreadline = async_create();
    async = input.onreadline;
    if (input.focus) input.focus();
  }
  else {
    async = async_create();
  }
  return async;
}


/*------------------------------------------------
  Formatting
------------------------------------------------*/
var $align = function(str,n,pad) { 
  if (n == undefined) return str; 
  n = n - str.length;
  if (n > 0) {
    if (pad == undefined) pad = " ";
    return (Array(n+1).join(pad) + str);
  }
  return str;
}

var $gformat = function(x,format) {
  var hex = /^[xX]([0-9]*)/.exec(format)
  if (hex) {
    var s = x.toString(16)
    if (format[0] == 'X') s = s.toUpperCase();
    return $align( s, hex[1], "0" )
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
function $error(s) {
  throw s;
}

function $primcatch(action,handler,_k) {
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

function $primfinally(action,handler,_k) {
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

function $pattern_match_error(location,definition) { 
  throw (location + (definition ? (": " + definition) : "") + ": pattern match failure"); 
};

/*------------------------------------------------
  Various
------------------------------------------------*/

var $int32_multiply = function(x,y) {
  var xhi = (x >> 16) & 0xFFFF;
  var xlo = x & 0xFFFF;
  var yhi = (y >> 16) & 0xFFFF;
  var ylo = y & 0xFFFF;
  var hi  = ((xhi * ylo) + (xlo * yhi));
  return (((hi << 16) + (xlo * ylo))|0)
}

var $int32_cmod = function(x,y) {
  if (y === 0) throw "modulus of zero";
  return ((x%y)|0);
}

var $int32_cdiv = function(x,y) {
  if (y === 0) throw "division by zero";
  return ((x/y)|0);
}

/*------------------------------------------------
  list helpers
------------------------------------------------*/

// Create a list with constant stack space
function $list(elems,tail) {
  var xs = tail;
  if (elems!=null && elems.length>0) { 
    for(var i = elems.length - 1; i >= 0; i--) {
      var elem = elems[i];
      xs = Cons(elem,xs);
    }
  }
  return xs;
}

// Create an array from a list with constant stack space
function $unlist(list) {
  var elems = [];
  while(list) {
    elems.push(list.head);
    list = list.tail;
  }
  return elems;
}

/*------------------------------------------------
  General javascript helpers
------------------------------------------------*/
// make a shallow copy
function $copy(obj) {
  if (typeof obj !== 'object') return obj;
  var value = obj.valueOf();
  if (obj != value) return new obj.constructor(value);
  var newobj = {};
  for( var prop in obj) newobj[prop] = obj[prop];
  return newobj;
}

// get the fields of an object
function $fields(obj) {
  var props = [];
  for (var prop in obj) props.push(prop);
  return props;
}

/* assign here so inlined primitives are available in system.core itself */
$std_core = { "intMultiply": $int32_multiply
            , "intCmod": $int32_cmod
            , "intCdiv": $int32_cdiv
            , "inBrowser": $inBrowser
            , "conslist": $list
            , "Yield": Yield
            }


// --------------------------------------------------------
// Reference effect handler.
// unfortunately, this is not tail-recursive. 
// In particular, the 'resume' reinvokes the our handler 
// without rewinding the stack.
// --------------------------------------------------------

function $makeHandler0(optag,ret,ops) {  
  function handler(yld,k) { 
    if (yld._tag === 1) { // Yield
      if (yld.op._tag === optag) {  // our operation?
        return ops(handler, 0, yld.cont, yld.op._field1, $resume0, k); 
      }
      else return $reyield0(yld, handler, k);
    }
    else return ret(yld.result,k);
  }
  return function(action,k) {
    if (k===undefined) k = id;
    return handler(action(Result),k);
  };
}

function $makeHandler1(optag,ret,ops) {  
  function handler(yld,loc,k) { 
    if (yld._tag === 1) { // Yield
      if (yld.op._tag === optag) {  // our operation?
        return ops(handler, 0, yld.cont, yld.op._field1, $resume1, loc, k); 
      }
      else return $reyield1(yld, handler, loc, k);
    }
    else return ret(loc, yld.result, k);
  }
  return function(loc,action,k) {
    if (k===undefined) k = id;
    return handler(action(Result),loc,k);
  };
}

function $resume0( handler, _, cont, x, k ) {
  return handler(cont(x),k);
}

function $resume1( handler, _, cont, loc, x, k ) {
  return handler(cont(x),loc,k);
}

function $reyield1( yld, handler, loc, k ) {
  var cont = yld.cont;
  yld.cont = function(x) { return handler(cont(x), loc, k); };
  return yld;
}

function $reyield0( yld, handler, k ) {
  var cont = yld.cont;
  yld.cont = function(x) { return handler(cont(x), k); };
  return yld;
}


// --------------------------------------------------------
// Optimized Tail-recursive handler.
// --------------------------------------------------------

function $makeOptHandler0(optag,ret,ops) { 
  function handler(yld,_k) { 
    var frame = { valid: true, resumed: false, k: _k };
    try {
      do {
        frame.resumed = false;            
        if (yld._tag === 1) {           // Yield operation
          if (yld.op._tag === optag) {  // our operation?
            yld = ops(frame, handler, yld.cont, yld.op._field1, $optResume0, frame.k); 
          }
          else {
            yld = $reyield0(yld, handler, frame.k); // todo: optimize unwinding/rewinding of reyielded ops
            // assert: frame.resumed===false
          }
        }
        else {
          yld = ret(yld.result, frame.k);
        }
      }
      while (frame.resumed);
      return yld;
    }    
    finally {
      frame.valid = false;
    }
  }

  return function(action,k) {
    if (k===undefined) k = id;
    return handler(action(Result),k);
  };
}

function $makeOptHandler1(optag,ret,ops) { 
  function handler(yld,_loc,_k) { 
    var frame = { valid: true, resumed: false, loc: _loc, k: _k };
    try {
      do {
        frame.resumed = false;            
        if (yld._tag === 1) {           // Yield operation
          if (yld.op._tag === optag) {  // our operation?
            yld = ops(frame, handler, yld.cont, yld.op._field1, $optResume1, frame.loc, frame.k); 
          }
          else {
            yld = $reyield1(yld, handler, frame,loc, frame.k); // todo: optimize unwinding/rewinding of reyielded ops
            // assert: frame.resumed===false
          }
        }
        else {
          yld = ret(frame.loc,yld.result, frame.k);
        }
      }
      while (frame.resumed);
      return yld;
    }    
    finally {
      frame.valid = false;
    }
  }

  return function(loc,action,k) {
    if (k===undefined) k = id;
    return handler(action(Result),loc,k);
  };
}

function $optResume0(frame,handler,cont,x,_k) {
  if (frame.valid) {
    frame.resumed = true;
    frame.k = _k;
    return cont(x);
  }
  else {
    return handler(cont(x),_k);
  }
}

function $optResume1(frame,handler,cont,_loc,x,_k) {
  if (frame.valid) {
    frame.resumed = true;
    frame.loc = _loc;
    frame.k   = _k;
    return cont(x);
  }
  else {
    console.log("invalid frame; reinvoke handler");
    return handler(cont(x),_loc,_k);
  }
}


// --------------------------------------------------------
// Optimized Tail-recursive generic handler.
// This uses a global stack of handlers with one handling
// (trampoline) routine.
//
// It tries to re-use the stack of handlers as much as 
// possible, including unwinding/rewinding of the 'reyields'
//
// Surprsingly, this implementation beats makeOptHandler even 
// for benchmarks that don't use reyields or nested handlers.
// --------------------------------------------------------
var $hstack = [];
var $htop    = -1;

function $copyHandler( h ) {
  return { optag: h.optag, ops: h.ops, ret: h.ret, 
           localCount: h.localCount, shallow: h.shallow,
           readonly: false,
           k: h.k, loc1: h.loc1, loc2: h.loc2, loc3: h.loc3, 
         };
}

var $identityHandler = {
  optag: "std/core/(.Eff-id)", 
  ret: function(x, _k) { return _k(x); }, 
  ops: function(_l1, _l2, _cont, _op, _resume , _k0 ) {
         return _resume(_l1, _l2, _cont, _unit_, _k0);
       },
  localCount: 0, shallow: false, readonly: true,
  k: id
};

function $pushStack(hstack,topbot) {
  var top    = topbot >> 16;
  var bottom = topbot & 0xFFFF;
  var top1   = top;
  if ($hstack===hstack && top-1 === $htop) {
    // console.log("reuse stack");
    $htop = bottom;  // push ourselves and any skipped handlers
  }
  else {
    console.log("copy stack, size: " + ($hstack.length+bottom-top+1).toString() );
    // create a new handler stack; todo: optimize this a bit more
    $hstack     = $hstack.slice(0,$htop+1);  // shallow copy top part
    var top1    = $hstack.length;
    var topush  = hstack.slice(top,bottom+1); // shallow copy the handlers we need to push
    if (topush.length <= 0) top1--;
    $hstack     = $hstack.concat(topush);     // and push them
    $htop       = $hstack.length-1;
  }

  if ($hstack[top1].readonly) {
    if ($hstack===hstack) {
      console.log("shallow copy due to read-only");
      $hstack = $hstack.slice(0); // shallow copy so it gets a different 'id' and we don't
                                // overwrite the 'readonly' handler
    }
    $hstack[top1] = $copyHandler($hstack[top1]); // deep copy this handler (and reset readonly)
  }
  return $hstack[top1];  
}

function $genOptResume0(hstack,topbot,cont,x,k) {
  var h = $pushStack(hstack,topbot);
  h.k = k;
  return cont(x);
}

function $genOptResume1(hstack,topbot,cont,loc1,x,k) {
  var h  = $pushStack(hstack,topbot);
  h.loc1 = loc1;
  h.k    = k;
  return cont(x);
}

function $genOptResume2(hstack,topbot,cont,loc1,loc2,x,k) {
  var h  = $pushStack(hstack,topbot);
  h.loc1 = loc1;
  h.loc2 = loc2;
  h.k    = k;
  return cont(x);
}

function $errorTooManyLocals(h) {
  error("Internal: too many locals in a handler definition for " + h.optag );
}
function $errorUnhandledOperation(yld) {
  error("Internal: unhandled operation in effect <" + (yld != null && yld.op != null ? yld.op._tag : "").replace(/\(\.Eff_(\w+)\)/,"$1") + ">");
}
function $errorNoResume(effname) {
  effname = (effname ? "the " + effname : "this");
  error("Internal: " + effname + " effect cannot be resumed");
}

// get some builtin effect tags
var $exnTag   = _Eff_hexn()._tag; 
var $asyncTag = _Eff_asyn()._tag; 

function $genericOptHandlerX( yld ) 
{
  do {
    if (yld._tag===1) {
      // pop handlers until we find an applicable handler
      var top     = $htop;
      var bottom  = top;
      while (top >= 0 && $hstack[top].optag !== yld.op._tag) {
        $hstack[top].readonly = true;
        top--;
      }
      // if not found: cannot happen in a typed setting, except for exception handlers and async
      if (top < 0) {
        if (yld.op._tag===$exnTag) {
          throw yld.op._field1._field1; // rethrow
        }
        else if (yld.op._tag===$asyncTag) {
          return _unit_;  // async goes all the way to main so returning _unit_ is ok.
        }
        else $errorUnhandledOperation(yld);
      }
      // pop current; we leave it on the stack for possible-reuse
      var h = $hstack[top];
      $htop = top-1;
      // shallow handlers don't re-apply the handler on resume
      if (h.shallow) {
        top++;  
      }
      // invoke
      var topbot = ((top & 0xFFFF) << 16) | (bottom & 0xFFFF);
      if (h.localCount === 0) {
        yld = h.ops($hstack,topbot, yld.cont, yld.op._field1,
                    $genOptResume0, h.k );
      }
      else if (h.localCount===1) {
        yld = h.ops($hstack, topbot, yld.cont, yld.op._field1, 
                    $genOptResume1, h.loc1, h.k );        
      }
      else if (h.localCount===2) {
        yld = h.ops($hstack, topbot, yld.cont, yld.op._field1, 
                    $genOptResume2, h.loc1, h.loc2, h.k );        
      }
      else $errorTooManyLocals(h);
    }
    else { // result
      // pop the handler
      var h = $hstack[$htop];
      $htop--;
      if (h.localCount===0) {
        yld = h.ret(yld.result,h.k);
      }
      else if (h.localCount===1) {
        yld = h.ret(h.loc1,yld.result,h.k);
      }
      else if (h.localCount===2) {
        yld = h.ret(h.loc1,h.loc2,yld.result,h.k);
      }
      else $errorTooManyLocals(h);
    }
  }
  while ($htop >= 0);
  $hstack = []; 
  return yld;
}

function $genericOptHandler( cont, x ) {
  while(true) {
    try {
      return $genericOptHandlerX(cont(x));
    }
    catch(exn) {
      // is there an hexn handler in the stack?
      var top     = $htop;
      while (top >= 0 && $hstack[top].optag !== $exnTag) {
        $hstack[top].readonly = true;
        top--;
      }
      if (top < 0) {
        // no handler; rethrow
        $hstack=[]; $htop=-1;
        throw exn;
      }
      else {
        // convert to a hexn effect and handle that as a non-resumable effect
        cont = function(_exn) { return hthrow(_exn, function(_) { errorNoResume("exn") }); };
        x = exn;
        // and re-invoke ourselves keeping the handler stack as is
      }
    }
  }
}

function $makeGenOptHandler0(optag,shallow,ret,ops) {
  return function(action,k) {
    var handler = { optag: optag, ret: ret, ops: ops, localCount: 0, shallow: shallow,
                    readonly: false, k : k || id};    
    return $withGenOptHandler(handler,action, k===undefined);
  }
}

function $makeGenOptHandler1(optag,shallow,ret,ops) {
  return function(loc1,action,k) {
    var handler = { optag: optag, ret: ret, ops: ops, localCount: 1, shallow: shallow,
                    readonly: false, k : k || id, loc1: loc1 };    
    return $withGenOptHandler( handler, action, k===undefined );
  };
}

function $makeGenOptHandler2(optag,shallow,ret,ops) {
  return function(loc1,loc2,action,k) {
    var handler = { optag: optag, ret: ret, ops: ops, localCount: 2, shallow: shallow,
                    readonly: false, k : k || id, loc1: loc1, loc2: loc2 };    
    return $withGenOptHandler( handler, action, k===undefined );
  };
}

function $withGenOptHandler( handler, action, startCps ) {
  function withHandler() {
    $hstack = $hstack.slice(0,$htop+1); // shallow copy since some resume may hold it 
    $hstack.push(handler);
    $htop++;    
    return ($htop === 0 ? $genericOptHandler(action,Result) : action(Result)); 
  }
  
  if (!startCps) {
    return withHandler();
  }
  else {  
    // startCps: sometimes we are called inside a cps context (with a $hstack)
    // but we are called in a total sub expression and need start our own fresh $hstack.
    var hstack;
    var htop;
    try {
      hstack  = $hstack; 
      htop    = $htop;
      $hstack = [];
      $htop   = -1;
      return withHandler();
    }
    finally {
      $hstack = hstack;
      $htop = htop;
    }
  }
}

// --------------------------------------------------------
// Support for asynchronous actions with effect handlers
// --------------------------------------------------------

function $asyncOn( async, cont ) {
  var hstack = $hstack; // make a shallow copy?
  var htop = $htop;

  async_on( async, function(x) {
    $hstack = hstack;
    $htop = htop;
    return $genericOptHandler( cont, x );
  });
  async_on_exn( async, function(exn) {
    $hstack = hstack;
    $htop = htop;
    return $genericOptHandler( function(_) { throw exn; }, _unit_ );
  });
  return Yield(_Eff_asyn(_Op_ayield), function(x){ return $errorNoResume("async"); });
}

function $asyncCallback0( k ) {
  var hstack = $hstack;
  var htop = $htop;
  return function() {
    $hstack = hstack;
    $htop = htop;
    return $genericOptHandler( k, _unit_ );
  };
}

function $asyncCallback1( k ) {
  var hstack = $hstack;
  var htop = $htop;
  return function(x) {
    $hstack = hstack;
    $htop = htop;
    return $genericOptHandler( k, x );
  };
}

function $asyncCallbackErr1( k ) {
  var hstack = $hstack;
  var htop = $htop;
  return function(err,x) {
    $hstack = hstack;
    $htop = htop;
    return $genericOptHandler( function(_) {
      if (err) throw err; 
      return k(x);      
    }, _unit_ );
  };
}

function $asyncCallbackErr2( k ) {
  var hstack = $hstack;
  var htop = $htop;
  return function(err,x1,x2) {
    $hstack = hstack;
    $htop = htop;
    return $genericOptHandler( function(_) {
      if (err) throw err; 
      return k(x1,x2);      
    }, _unit_ );
  };
}

function $asyncAll( actions, k ) {
  var n       = actions.length;
  var results = new Array(n);
  var exn;
  if (n<=0) return k(results);
  
  // all concurrent actions end with this continuation
  // only when all are finished, we resume the passed in continuation
  function allk(i,x,f) {
    results[i] = x;
    n--;
    if (n <= 0) {
      // if an exception was set, raise it now
      if (exn!==undefined) throw exn;
      // in case a branch exits more than once (due to amb for example)
      for(i = 0; i < results.length; i++) {
        if (results[i] === undefined) return f(x); 
      }
      return k(results);
    }
    else {
      return f(x);
    }
  }

  for( var i = 0; i < n; i++) {
    (function(j){ 
      var cb = $asyncCallback0( function(_) {
        return $primcatch( function(k1) {
            return actions[j](k1);
          },
          function(_exn,_k2) {
            if (exn===undefined) exn = _exn;
            return allk(j,undefined,ayield);
          },
          function(x) { 
            return allk(j,x,Result); 
          });
        }); 
      // by using setTimeout we can schedule all continuations for execution
      setTimeout(cb,0); 
    })(i);  // capture i by value
  }
  return ayield();
}

// --------------------------------------------------------
// Reference generic handler; use a separate stack of handlers
// This is a non-tail-recursive version
// --------------------------------------------------------


function $genResume0(popped,_,cont,x,k) {
  var current = popped[popped.length-1];
  current.k   = k;
  $hstack     = popped.concat($hstack);
  return cont(x);
}

function $genericHandler( yld ) 
{
  do {
    if (yld._tag===1) {
      // pop handlers until we find an applicable handler
      var popped = [];
      while ($hstack[0].optag !== yld.op._tag) {
        popped.push( $hstack.shift() );
      }
      // pop current
      var current = $hstack.shift();
      popped.push(current);
      // invoke
      yld = current.ops( popped, 0, yld.cont, yld.op._field1, $genResume0, current.k );
      if ($hstack.length===0) return yld;
    }
    else { // result
      var current = $hstack.shift();
      yld = current.ret(yld.result,current.k);
      if ($hstack.length===0) return yld;
    }
  }
  while (true);
}

function $makeGenHandler0(optag,ret,ops) {
  return function(action,k) {
    if (k===undefined) k = id;
    $hstack.unshift({ optag: optag, ret: ret, ops: ops, k : k });
    if ($hstack.length===1) {
      return $genericHandler(action(Result));
    }
    else {
      return action(Result);
    }
  }
}



