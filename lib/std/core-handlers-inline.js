/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


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

function _show_handler(h) {
  return ("handler: " + h.optag + ", ro: " + h.readonly + ", lcnt: " + h.localCount +", shallow: " + h.shallow);
}
function _log_handlers(msg,ctx) {
  if (!ctx) ctx = { htop: $htop, hstack: $hstack };
  console.log(msg)
  console.log(" $htop=" + ctx.htop.toString());
  console.log(" $hstack=[\n  " + ctx.hstack.map(_show_handler).join(",\n  ") + "]\n")
}

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
    // console.log("copy stack, size: " + ($hstack.length+bottom-top+1).toString() );
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
      // console.log("shallow copy due to read-only");
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
function $showEffectTag(efftag) {
  return (efftag || "").replace(/\(\.Eff_(\w+)\)/,"$1");
}

function $errorUnhandledOperation(eff) {
  error("Internal: unhandled operation in effect <" + $showEffectTag(eff != null ? eff._tag : "") + ">");
}
function $errorNoResume(effname) {
  effname = (effname ? "the " + effname : "this");
  error("Internal: " + effname + " effect cannot be resumed");
}

function $Unwind(tag,result) {
  this.name    = 'std/core/Unwind';
  this.result  = result;
  this.message = "unwinding stack for linear effect " + $showEffectTag(tag);
  this.stack   = (new Error()).stack;
}
$Unwind.prototype = Object.create(Error.prototype);
$Unwind.prototype.constructor = $Unwind;

var _tag_Eff_async = "std/async/(.Eff-async)";

function $opNotFound(op) {
  if (op._tag===_tag_Eff_async) {   
    return _unit_;  // async goes all the way to main so returning _unit_ is ok.
  }
  else if (op._tag===_tag_Eff_hexn) {
    throw op._field1._field1; // rethrow 
  }
  else {
    return $errorUnhandledOperation(op); // will throw
  }
}

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
      if (top < 0) return $opNotFound(yld.op);
      // pop current; we leave it on the stack for possible-reuse
      var h = $hstack[top];
      $htop = top-1;
      // shallow handlers don't re-apply the handler on resume
      if (h.shallow) top++;
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
      if (exn instanceof $Unwind) {
        // unwinding the stack for a returning linear effect; $htop is already set correctly
        if ($htop < 0) {
          // final result, return
          return exn.result;
        }
        else {
          // propagate the result to the next handler
          cont = function(_x) { return Result(_x); }
          x = exn.result;
          // and reinvoke ourselves
        }
      }
      else {
        // is there an hexn handler in the stack?
        var top     = $htop;
        while (top >= 0 && $hstack[top].optag !== _tag_Eff_hexn) {
          $hstack[top].readonly = true;
          top--;
        }
        if (top < 0) {
          // no handler; rethrow
          // _log_handlers("genericOptHandler: rethrow exception");        
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
}

// Used for async to save the current handler stack
function _capture_handler_context(upto) {
  var ctx = { hstack: $hstack, htop: $htop, upto: upto };
  ctx.log = function(msg) { _log_handlers(msg,ctx); };
  return ctx;
}

// Used for async to continue a callback with a saved handler stack
function _invoke_in_handler_context( ctx, cont, x ) {
  $hstack = ctx.hstack;
  $htop = ctx.htop;
  return ($htop < 0 ? $genericOptHandler( cont, x ) : cont(x));
}

// Used for async to continue a callback with a saved handler stack
function _resume_in_handler_context( ctx, resume, x, _k ) {
  $htop = ctx.htop;
  if (ctx.upto <= 0) {
    $hstack = ctx.hstack;
  }
  else {
    var i = ctx.upto;
    while(i < ctx.hstack.length) {
      $hstack[i] = ctx.hstack[i];
      i++;
    }
  }
  function cont(_x) { return resume( _x, _k ); }
  return ($htop < 0 ? $genericOptHandler(cont,x) : cont(x));
}

// Used for async when resumes escape the trampoline 
function _outer_resume( resume ) {
  function cont(_x) { return resume( _x, id ); }
  return (function(x) {
    return ($htop < 0 ? $genericOptHandler(cont,x) : cont(x));
  }); 
}

// --------------------------------------------------------
// Create a handler
// --------------------------------------------------------


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
    // _log_handlers("pushed new handler");
    // in the case of a linear effect, the handler action is not always CPS translated
    // here we do dynamic reflection to transform it into CPS :-(
    var xaction = (action.length >= 1 ? action : function(_k) { return _k(action()); });
    // and invoke it, possible starting the trampoline
    return ($htop === 0 ? $genericOptHandler(xaction,Result) : xaction(Result)); 
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
// Experiment with linear effects
// --------------------------------------------------------

function $errorNonLinear(h) {
  error("Internal: linear effect is resumed multiple times: " + $showEffectTag(h.optag));
}
function $errorLinearNotResumed(efftag,op) {
  error("Internal: linear effect is never resumed: " + $showEffectTag(h.optag));
}
function $resume_linear0(hstack,top,cont,x,k) {
  var h = hstack[top];
  if (h.linearCount++ > 0) $errorNonLinear(h);
  h.k = k;
  return cont(x);
}

function $resume_linear1(hstack,top,cont,loc1,x,k) {
  var h  = hstack[top];
  if (h.linearCount++ > 0) $errorNonLinear(h);
  h.loc1 = loc1;
  h.k    = k;
  return cont(x);
}

function $yield_linear( eff ) {
  return $yield_linearx( eff._tag, eff._field1 );
}

function $yield_linearx( efftag, op) {
  // find our handler
  var top     = $htop;
  var bottom  = top;
  while (top >= 0 && $hstack[top].optag !== efftag) {
    $hstack[top].readonly = true;        
    top--;
  }
  // if not found: cannot happen in a typed setting
  if (top < 0) $errorUnhandledOperation({_tag:efftag,_field1:op});
  
  // pop current; we leave it on the stack for possible-reuse
  // note: we need to do this in case the handler throws or invokes other (linear) operations itself
  var h = $hstack[top];
  $htop = top-1;

  // invoke our handler directly
  var cont = id;
  var res;
  h.linearCount = 0;
  if (h.localCount===0) {
    res = h.ops($hstack, top, cont, op, $resume_linear0, h.k);
  }
  else if (h.localCount===1) {
    res = h.ops($hstack, top, cont, op, $resume_linear1, h.loc1, h.k);
  }
  else {
    $errorTooManyLocals(h);
  }
  if (h.linearCount <= 0) {
    // we did not resume! unwind the stack and return the final result
    // note: this goes wrong if a regular handled operation is executed and returns a yld
    //       todo: we should check this in the type system.
    throw new $Unwind(efftag,res);
  }
  else if (h.linearCount > 1) {
    // todo: prevent this in the type system
    $errorNonLinear(h); // should not happen as it is checked inside the resume_linear functions
  }
  // restore the handler stack
  $htop = bottom;
  return res;
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
    //console.log("invalid frame; reinvoke handler");
    return handler(cont(x),_loc,_k);
  }
}

