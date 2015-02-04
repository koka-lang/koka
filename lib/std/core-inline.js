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
      nodein = readline.createInterface( { input: process.stdin, output: process.stdout });
      nodein.print = nodeout.print;
      nodein.on('line', function(s) {
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

function $primcatch(action,handler) {
  try {
    return action();
  }
  catch(exn) {
    return handler(exn)
  }
}

function $finally(action,handler) {
  try {
    return action();
  }
  finally {
    handler();
  }
}

function $pattern_match_error(location,definition) { 
  throw(location + (definition ? (": " + definition) : "") + ": pattern match failure"); 
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
            }

