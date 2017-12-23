/*---------------------------------------------------------------------------
  Copyright 2017 Daan Leijen, Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
  Copyright 2017 Daan Leijen & Manuel Serrano

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


"use strict";

const Eff = (function(){

/*---------------------------------------------------------------------------
General notes:

- _functions starting with an underscore are meant to be private

General design:

* Bind and Yield:
  - To get results from effectful functions use `bind`. For example, `return bind( get(), function(i) {... })`
  - Effectful functions either return with their result, or they return a _yield_ object.
  - Yielding consists of returning an object of the form:
    ```
    { cont      : a -> b  // the continuation to resume with a result for the yielded operation (like `moveNext`)
      op_arg    : c       // the operation argument (like an integer for a `set` operation)
      handler   : handler // the handler that handles the effect
      branch    : branch  // the branch in the handler that handles this operation
    }
    ```
    where the operation has type, `c -> a`, and the action argument to the handler has a
    result type `b`.
  - A `bind` operation extends the continuation field `cont` to be able to resume correctly

* Handlers and the handler stack:
  - A handler is described as:
    ```
    { effect_tag: string    // the effect being handled, like `amb`
      return_fun: function  // a function applied to the result of the action passed to the handler
      branches  : [branch]  // a vector of branches, one for each operation in the effect
    }
    ```
  - Handlers are pushed on the global handler stack when enabled
  - When an operation yields, it first finds the innermost handler on this handler stack 
    and also the branch that handles the particular operation.

* Branches
  - A branch handles a particular operation:
    ```
    { op_tag     : string,   // the operation tag being handled, like `get` or `set`
      resume_kind: int,      // the kind of resume: a tail-resumptive branch or a regular one? 
      branch_fun : (resume,op_arg,local) -> a   // the actual handling function for `op_tag`
    }
    ```

* Tail resumptive branches:
  - For branches that are _tail resumptive_, we ensure that we yield them to the
    handlers one up to enable running in constant stack space (even through binds).

* Exceptions
  - todo

---------------------------------------------------------------------------*/

// For assertions
function _NO_assert(b,msg) {}

function _DO_assert(b,msg) {
  if (!b) {
    msg = msg || "no message";
    throw new Error("Assertion failed: " + msg );
  }
}

function _assert(b,msg) {
  _NO_assert(b,msg);
}

function id(x) {
  return x;
}

// Different resume kinds for branches: used for potential optimizations (`Normal` is always correct to use).
// Especially branches that `Tail` resume are very common and should be optimized well: in this library
// these are resolved to direct method calls without needing to unwind/rewind the stack.
const _resume_kind = {
  Never   : 0,    // never calls `resume`.
  Tail    : 1,    // calls `resume` at most once as the last thing to do (or does not resume at all). (and the handler is not `Shallow`)
  Once    : 2,    // calls `resume` at most once.
  Normal  : 3,    // first-class resumption: calls `resume` once or many times.
  Shallow : 4     // the `resume` does not resume under the handler itself.
};


function Yield( handler, cont, op_arg, branch ) {
  this.cont    = (cont != null ? cont : _cont_id);
  this.op_arg  = op_arg;
  this.handler = handler;
  this.branch  = branch;
}

function _cont_id( arg, exn ) {
  if (exn != null) throw exn;
  return arg;
}

/*----------------------------------------------------------------
  Bind, yield
----------------------------------------------------------------*/

// Bind a result of an effectful computation to the `next` function. 
// When the effectful computation yielded on operation, we can remember where to continue (namely the `next` function)
function bind( x, next ) {
  if (x instanceof Yield) {
    if (x.branch.resume_kind !== _resume_kind.Never) {
      const cont = x.cont; // capture locally
      x.cont = function(arg,exn) { return bind(cont(arg,exn),next); };
    }
    return x;
  }
  else {
    return next(x);
  }
}


// Yielding an operation consists finding the handler and returning a yield object.
// The `_resume_tail_count` counter ensures that every once in a while
// we fully yield a tail-resumptive operation instead of optimizing so we unwind the stack
// of `bind`'s and ensure tail-recursive functions with `bind`s do not run out of stack space.
let _resume_tail_count = 0;
let _max_resume_tails  = 100;

function yield_op( effect_tag, op_tag, op_arg ) {
  // find our handler in the global handler stack
  let handler;
  const n = _handlers.length;
  let i;
  for(i = n - 1; i >= 0; i--) {
    handler = _handlers[i];
    if (handler.effect_tag === effect_tag) break;
    i -= handler.skip; // skip over skip frames
  }
  if (i<0) return _yield_no_handler(effect_tag,op_tag);
  const skip   = n - i;
  const branch = handler.branches[op_tag];
  if (branch==null) return _yield_no_branch(effect_tag,op_tag);
  
  const is_linear = handler.is_linear;
  if (is_linear || (branch.resume_kind === _resume_kind.Tail && _resume_tail_count < _max_resume_tails)) {
    if (!is_linear) _resume_tail_count++;
    // The handler uses a tail resumption: optimize it by directly 
    // calling the branch function without any unwinding.
    // This is important in practice as 95% of all operation handlers are tail-resumptions.    
    //return _call_tail_branch(handler,branch,skip,op_arg);  // Inline for performance 
    const skipframe = _cached_skip_frame(skip);
    _handlers_push(skipframe);
    let res;
    try {
      res = branch.branch_fun( _resume_tail_return, op_arg, handler.local );  
    }
    catch(exn) {
      // an exception was raised; we unwind back to the handler and reraise the exception there.
      throw new FinalizeException( handler, function() { throw exn; } );
    }
    finally {
      _handlers_pop(skipframe);
    }
    if (res instanceof _TailResume) {
      if (res.local !== undefined) handler.local = res.local;
      return res.result;
    }
    else {
      return _skip_handle_yield( handler, res, skipframe );     
    }
  }
  else {
    _resume_tail_count = 0;
    return new Yield(handler,_cont_id,op_arg,branch);
  }
}

function _yield_no_branch(effect_tag, op_tag) {
  throw new Error("bad handler: operation " + op_tag + " is not found in the handler for " + effect_tag);
}

function _yield_no_handler(effect_tag, op_tag) {
  throw new Error("there is no handler for " + effect_tag + "/" + op_tag);
}


/*----------------------------------------------------------------
  The handler stack
----------------------------------------------------------------*/

// The global handler stack
const _handlers = [];

function _handlers_push(h) {
  _handlers.push(h);
}

function _handlers_pop(expected) {
  const h = _handlers.pop();
  _assert(h === expected, "Handler stack is out of sync!");
}

function _handlers_top(idx_from_top) {
  return _handlers[_handlers.length - 1 - idx_from_top];
}


/* ----------------------------------------------------------------
  Skip frames: skip on a first read :-)

  When doing direct calls to tail-resumptive branches, we push
  a skip frame to ensure that any operations in a handler are not handled
  by any operations under it in the handler stack.

  There are other approaches, like popping and restorign the handler stack,
  but this seems most efficient?
----------------------------------------------------------------*/

// Special effect tag of skip handlers
const _effect_skip = "<skip>";

// Cache allocation of common skip frames
function _new_skip_frame(skip) {
  return new Handler("<skip>",id,[],undefined,skip);
}

const _skip_frame1 = _new_skip_frame(1);
const _skip_frame2 = _new_skip_frame(2);
const _skip_frame3 = _new_skip_frame(3);

function _cached_skip_frame(skip) {
  if (skip===1)      return _skip_frame1;
  else if (skip===2) return _skip_frame2;
  else if (skip===3) return _skip_frame3;
  else return _new_skip_frame(skip);
}


// Common case: directly invoke a branch without unwinding on tail-resumptive branches
function _call_tail_branch(handler,branch,skip,op_arg) {
  _assert(_handlers_top(skip-1) === handler);
  const skipframe = _cached_skip_frame(skip);
  _handlers_push(skipframe);
  var res;
  try {
    res = branch.branch_fun( _resume_tail_return, op_arg, handler.local );  
  }
  catch(exn) {
    // an exception was raised; we unwind back to the handler and reraise the exception there.
    throw new FinalizeException( handler, function() { throw exn; } );
  }
  finally {
    _handlers_pop(skipframe);
  }
  return _skip_handle_yield( handler, res, skipframe );
}

function _TailResume(result,local) {
  this.result = result;
  this.local = local;
}

function _resume_tail_return(result,local) {
  return new _TailResume(result,local);
}

// Resume in a tail-resumptive branch 
function _skip_resume( handler, skipframe, cont, arg, exception ) {
  _handlers_push(skipframe);
  var res;
  try { 
    res = cont(arg,exception);
  }
  catch(exn) {
    // an exception was raised; we unwind back to the handler and reraise the exception there.
    throw new FinalizeException( handler, function() { throw exn; } );
  }
  finally {
    _handlers_pop(skipframe);
  }
  return _skip_handle_yield( handler, res, skipframe );
}

// Handle the result of a tail-resumptive branch 
function _skip_handle_yield( handler, res, skipframe ) {
  _assert( _handlers_top(skipframe.skip - 1) === handler, "skipframe does not match handler");
  if (res instanceof _TailResume) {
    if (res.local !== undefined) handler.local = res.local;
    return res.result;
  }
  else if (res instanceof Yield) {
    // extend the continuation with the skip frame 
    const yld = res;
    if (yld.branch.resume_kind !== _resume_kind.Never) {
      const cont = yld.cont; // capture locally
      yld.cont = function(arg,exn) {
        return _skip_resume( handler, skipframe, cont, arg, exn );
      }
    }
    // and reyield
    return yld;
  }
  else {
    // we returned normally without resuming!
    // in this case we need to unwind back to the handler and return from there.
    throw new FinalizeException(handler, function() { return res; });
  }
}

// A special exception to yield from linear effects (where no binds are used)
function FinalizeException( handler, handle ) {
  this.handler = handler;
  this.handle = handle;
  this.message = "Internal finalize exception";
  if ("captureStackTrace" in Error) {
    Error.captureStackTrace(this,FinalizeException);  // best on Node.js
  }
  else {
    this.stack = (new Error()).stack; // in browsers
  }
}
FinalizeException.prototype = Object.create(Error.prototype);
FinalizeException.prototype.constructor = FinalizeException;


/*----------------------------------------------------------------
  Create an operation branch; i.e. one operation match in a handler
----------------------------------------------------------------*/

// Create an handler branch, i.e. the function that is called for a particular operation
// The `branch_fun` takes 3 arguments: 
//   (resume,op_arg,local)
// where `op_arg` the operation and `local` the current local state of a parameterized handler. 
// The `resume` function takes 2 arguments: 
//   (result,new_local)
// where `result` is the resume result, and `new_local` the new local state of the parameterized handler. 
function Branch( resume_kind, op_tag, branch_fun ) {
  this.resume_kind = resume_kind;
  this.op_tag      = op_tag;
  this.branch_fun  = branch_fun;
}

function new_branch( resume_kind, op_tag, branch_fun ) {
  return new Branch(resume_kind, op_tag, branch_fun );
}

function new_branch_never( op_tag, branch_fun ) {
  return new_branch(_resume_kind.Never,op_tag,branch_fun);
}

function new_branch_once( op_tag, branch_fun ) {
  return new_branch(_resume_kind.Once,op_tag,branch_fun);
}

function new_branch_tail( op_tag, branch_fun ) {
  return new_branch(_resume_kind.Tail,op_tag,branch_fun);
}

function new_branch_normal( op_tag, branch_fun ) {
  return new_branch(_resume_kind.Normal,op_tag,branch_fun);
}

function new_branch_shallow( op_tag, branch_fun ) {
  return new_branch(_resume_kind.Shallow,op_tag,branch_fun);
}




/*----------------------------------------------------------------
  Create a handler
----------------------------------------------------------------*/

// A _linear_ handler promises that all its branches are always
// `Tail` or `Never`; such linear effects don't need any `bind`s and
// can be implemented more efficiently: In the simple library we 
// do not support this though.
function new_handler_linear( effect_tag, return_fun, branches ) {
  return new_handler(effect_tag,return_fun,branches,true);
}


function Handler(effect_tag, return_fun, branches, local, skip, is_linear) {
  this.effect_tag = effect_tag;
  this.return_fun = return_fun;
  this.branches   = branches;
  this.local      = local;
  this.skip       = (skip ? skip : 0);
  this.is_linear  = (is_linear ? true : false);
}

// Create a handler for a given effect
function new_handler( effect_tag, return_fun, branches0, is_linear ) 
{
  // initialize the branches such that we can index by `op_tag`
  // regardless if the `op_tag` is a number or string.
  const branches = new Array(branches0.length);
  for(let i = 0; i < branches0.length; i++) {
    const branch = branches0[i];
    branches[branch.op_tag] = branch;
    if (typeof op_tag !== "number") branches[i] = branch;
  }

  // shared handler if there is no local state
  const shared_handler = new Handler(effect_tag, return_fun, branches);
  
  // return a handler function: `action` is executed under the handler.
  return (function(action,local) {
    const handler = (local === undefined ? shared_handler : 
                      new Handler(effect_tag, return_fun, branches, local, 0, is_linear)); 
    function yaction() { return yield_iter(action()); }; // auto convert from iterators
    return _handle_action(handler, yaction );
  });
}

function new_handler1( effect_tag, return_fun, branches, is_linear ) {
  const f = new_handler(effect_tag, return_fun, branches, is_linear );
  return (function(local,action) { return f(action,local); });
}

// Run action under a handler
function _handle_action( handler, action ) {
  _handlers_push(handler);
  var result;
  try {
    if (handler.return_fun==null) {
      result = action();
    }
    else {
      // todo: slightly wrong as operations in the return clause are handled by our handler.
      result = bind(action(), function(x) { return handler.return_fun(x,handler.local); });
    }
  }
  catch(exn) {
    if (exn instanceof FinalizeException && exn.handler === handler) return exn.handle();
    throw exn; // rethrow
  }
  finally {
    _handlers_pop(handler);
  }    
  return _handle_yield( result, handler);
}

// Resume under a handler
function _handle_resume(handler,cont,arg,exn,local) {
  if (local !== undefined) handler.local = local;
  return _handle_yield( _resume_cont(handler,cont,arg,exn), handler);
}

function _resume_cont(handler,cont,arg,exn) {
  var result;
  _handlers_push(handler);
  try {
    result = cont(arg,exn);    
  }
  catch(exn) {
    if (exn instanceof FinalizeException && exn.handler === handler) return exn.handle();
    throw exn;
  }
  finally {
    _handlers_pop(handler);
  }        
  return result;
}


// Catch an operation branch resuming while promising to never resume
function _resume_never(arg,local) {
  throw new Error("A handler branch marked as never-resuming tried to resume.")
}



/*----------------------------------------------------------------
  The core handling function of operations
----------------------------------------------------------------*/

// Process the result of an action or resumption
function _handle_yield(result, handler) {
  var tailresumed;
  do {
    tailresumed = false;
    if (result instanceof Yield) {
      if (result.handler !== handler) {
        // not handled by us, update the continuation and reyield
        if (result.branch.resume_kind !== _resume_kind.Never) {
          const cont = result.cont;         // capture locally
          const local = handler.local;
          result.cont = function(arg,exn) { 
            return _handle_resume(handler,cont,arg,exn,local); 
          };          
        }
      }
      else {
        // handle the operation
        const local = handler.local;  // capture locally
        const branch = result.branch; 
        const cont   = result.cont;
        const op_arg = result.op_arg;
        const resumed = { resumed: false}; // out parameter
        try {
          result = _call_branch(handler, branch, cont, op_arg, local, resumed );
        }
        catch(exn) {
          if (resumed.resumed) throw exn; // rethrow if already resumed once
          // otherwise run finalizers in the contiunation first
          _finalize(handler, cont, new FinalizeException(handler, function(){ throw exn; }));
          throw exn; // paranoia
        }
        if (!resumed.resumed && !result instanceof Yield) {
          // returned without resuming; run finalizers in the continuation first
          result = _finalize(handler, cont, new FinalizeException(handler, function(){ return result; }));
        }
        tailresumed = (resumed.resumed && branch.resume_kind === _resume_kind.Tail);
      }
    }
  }
  while(tailresumed);
  return result;
}


function _call_branch(handler, branch, cont, op_arg, local, resumed ) {
  if (branch.resume_kind === _resume_kind.Tail) {
    return branch.branch_fun( function(arg,local) {
                                resumed.resumed = true;
                                if (local !== undefined) handler.local = local;
                                return _resume_cont(handler,cont,arg,undefined); // and repeat the while(tailresumed)
                              }, op_arg, local);
  }
  else if (branch.resume_kind === _resume_kind.Never) {
    return branch.branch_fun( _resume_never, op_arg, local );
  }    
  else if (branch.resume_kind === _resume_kind.Shallow) {
    return branch.branch_fun( function(arg,local) {
                                resumed.resumed = true;                     
                                return cont(arg,undefined);
                              }, op_arg, local);
  }
  else {
    return branch.branch_fun( function(arg,local) { 
                                resumed.resumed = true; 
                                return _handle_resume(handler,cont,arg,undefined,local); 
                              }, op_arg, local );
  }
}

function _finalize(handler,cont,exception) {
  try {
    return cont(undefined,exception);
  }
  catch(exn) {
    if (exn instanceof FinalizeException && exn.handler === handler) return exn.handle();
    throw exn;
  }
}

/*----------------------------------------------------------------
  Extension: make it possible to catch exceptions over effectful 
  computations; in particular, this scopes over the `next` continuations
  of the `bind`s.
----------------------------------------------------------------*/

// An exception handler. 
// There is no need to push on the handler stack since there is 
// no associated operation (i.e. it is handled by the runtime system already)
function handle_exn( action, on_exn, on_final ) {
  return _handle_exn_resume( action, undefined, undefined, on_exn, on_final);
}

function _handle_exn_resume( cont, arg, exception, on_exn, on_final) {
  const n = _handlers.length; // remember the handler stack length so we can restore it on an exception
  try {
    const result = cont(arg,exception);
    _assert(_handlers.length === n, "Handler stack is out of sync in a try block");
    if (result instanceof Yield) {
      const cont = result.cont; // capture locally
      result.cont = function(arg,exn) {
        return _handle_exn_resume(cont, arg, exn, on_exn, on_final);  // resume within the try block
      };
    }   
    return result;
  }
  catch(exn) {
    _assert(_handlers.length >= n, "After exception the handlers stack is too shallow");
    _assert(_handlers.length === n, "After exception the handlers stack was not in sync");
    if (_handlers.length > n) { _handlers.length = n; } // restore handler stack     
    if (exn instanceof FinalizeException) {
      throw exn; // rethrow internal exception
    }
    else if (on_exn == null) {
      throw exn; // rethrow if no handler
    }
    else {
      return on_exn(exn);
    }
  }
  finally {
    if (on_final != null) on_final();
  }
}


/*----------------------------------------------------------------
  Extension: functions to work with generator style yielding of operations.
----------------------------------------------------------------*/

// Transform the result of an effectful iterator (i.e. where each operation is `yield`ed) to regular operation yields.
function yield_iter( x ) {
  return (_is_iterator(x) ? _yield_iter_resume(x,undefined) : x);
}

// Is this an iterator?
function _is_iterator(x) {
  return (x != null && typeof x.next === "function");
}

// Handle operation yields that are yielded through an iterator.
function _yield_iter_resume( iter, arg, exception ) {
  let res = (exception != null ? iter.throw(exception) : iter.next(arg));
  if (!res.done) {
    const yld = res.value;
    _assert(yld instanceof Yield, "regular operations should be wrapped in `to_iter` to lift them to an iterator (function*)");
    if (yld.branch.resume_kind > _resume_kind.Once) return _yield_iter_not_once(yld);
    const cont = yld.cont;  // capture locally 
    yld.cont = function(argval,exn) {
      if (cont === _cont_id) {
        return _yield_iter_resume(iter,argval,exn);
      }
      else {
        return bind(cont(argval,exn), function(bindarg) {  
          return _yield_iter_resume(iter,bindarg);
        });
      }
    };
  }
  return res.value;
}

function _yield_iter_not_once(yld) {
  throw new Error("Effects yielded in iterators should resume at most once! (" + yld.handler.effect_tag + ")");
}

// Use this instead of a `yield` keyword to optimize for tail-resumptive operations that might
// return a value right away.
function* to_iter( x ) {
  if (x instanceof Yield) {
    return yield x;
  }
  else {
    return x;
  }
}


function _enable_tail_resume_optimization(enable) { 
  _max_resume_tails = (enable ? 100 : 0);
}

/*----------------------------------------------------------------
  Exports
----------------------------------------------------------------*/
return {
  // core functions
  yield_op    : yield_op,
  bind        : bind,
  new_branch  : new_branch,
  new_handler : new_handler,
  new_handler1: new_handler1,
  handle_exn  : handle_exn,
  
  // support iterators
  to_iter     : to_iter,
  yield_iter  : yield_iter,

  // convenience
  new_handler_linear  : new_handler_linear,
  new_branch_never    : new_branch_never,
  new_branch_tail     : new_branch_tail,
  new_branch_once     : new_branch_once,
  new_branch_normal   : new_branch_normal,
  new_branch_shallow  : new_branch_shallow,
  
  // unsafe debug/extension exports
  Yield      : Yield,
  _handlers  : _handlers,
  _enable_tail_resume_optimization: _enable_tail_resume_optimization,
};

})(); // Eff

// extend std_core with these primitives
$std_core = _export($std_core, {
  _yieldop      : Eff.yield_op,
  _bind         : Eff.bind,
  _new_branch   : Eff.new_branch,
  _new_branch1  : Eff.new_branch,
  _new_handler  : Eff.new_handler,
  _new_handler1 : Eff.new_handler1,
  _handle_exn   : Eff.handle_exn
});


