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

var Eff = (function() {
/*---------------------------------------------------------------------------
  Copyright 2017 Daan Leijen & Manuel Serrano

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

"use strict";
"use strict";

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
    { effect_name: string    // the effect being handled, like `amb`
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

function _assert(b,msg) {
  //msg = msg || "no message";
  //throw new Error("Assertion failed: " + msg );
}

function id(x) {
  return x;
}

// Different resume kinds for branches: used for potential optimizations (`Normal` is always correct to use).
// Especially branches that `Tail` resume are very common and should be optimized well: in this library
// these are resolved to direct method calls without needing to unwind/rewind the stack.
const _resume_kind = {
  Never      : 0,    // never calls `resume`.
  Tail       : 1,    // calls `resume` at most once as the last thing to do (or does not resume at all). (and the handler is not `Shallow`)
  ScopedOnce : 2,    // at most once, and in the scope of the branch
  Scoped     : 3,    // calls `resume` in the scope of the branch
  Once       : 4,    // calls `resume` at most once, perhaps outside the scope of the branch.
  Normal     : 5,    // first-class resumption: calls `resume` once or many times.
};

const _handler_kind = {
  Deep   : 0,
  Linear : 1,
  Shallow: 2,

  _Skip  : -1,
  _Inject: -2,
};

function Yield( handler, branch, op_arg ) {
  this.cont    = _cont_id;
  this.op_arg  = op_arg;
  this.handler = handler;
  this.branch  = branch;
}


function YieldException(exn) {
  this.exn = exn;
}

function _cont_id( arg ) {
  if (arg instanceof YieldException) throw arg.exn;
  return arg;
}

/*----------------------------------------------------------------
  Bind, yield
----------------------------------------------------------------*/

// Bind a result of an effectful computation to the `next` function.
// When the effectful computation yielded on operation, we can remember where to continue (namely the `next` function)
function bind( x, next ) {
  if (x instanceof Yield) {
    const cont = x.cont; // capture locally
    x.cont = function(arg) { return bind(cont(arg),next); };
    return x;
  }
  else {
    return next(x);
  }
}

function bind_const( x, y ) {
	if (x instanceof Yield) {
    const cont = x.cont; // capture locally
    x.cont = function(arg) { return bind_const(cont(arg),y); };
    return x;
  }
  else return y;
}

// Yielding an operation consists finding the handler and returning a yield object.
// The `_resume_tail_count` counter ensures that every once in a while
// we fully yield a tail-resumptive operation instead of optimizing so we unwind the stack
// of `bind`'s and ensure tail-recursive functions with `bind`s do not run out of stack space.
let _resume_tail_count = 0;
let _max_resume_tails  = 100;

function _handlers_find( effect_name, handler_tag, op_name ) {
  // find our handler in the global handler stack
  if (handler_tag===undefined) handler_tag = 0;
  const n = _handlers.length;
  let inject = 0;
  let i;
  // for all handlers..
  for(i = n - 1; i >= 0; i--) {
    const handler = _handlers[i];
    if (handler.effect_name === effect_name &&
        (handler_tag === 0 || handler.handler_tag === handler_tag))
    {
      // we match on the handler effect + handler_tag
      if (handler.handler_kind===_handler_kind._Inject) inject++;   // inject effect
      else if (inject > 0) inject--;     // skip over this handler due to previous injection
      else return i;
    }
    i -= handler.skip; // skip over skip frames
  }
  const msg = "use of " + (handler_tag ? "a resource" : "an operation") +
               " outside of its handler scope: " + _show_effect(effect_name,handler_tag) + "/" + op_name;
  throw new Error(msg);
}

function yield_op( effect_name, op_name, op_arg, op_tag, handler_tag ) {
  if (op_tag==null) op_tag = op_name;
  // find our handler in the global handler stack
  const i      = _handlers_find(effect_name, handler_tag, op_name);
  const handler= _handlers[i];
  const skip   = _handlers.length - i;
  const branch = handler.branches[op_tag];
  if (branch==null) throw new Error("bad handler: operation " + op_name + " is not found in the handler for " + _show_effect(effect_name,handler_tag));

  const is_linear = (handler.handler_kind === _handler_kind.Linear);
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
      throw new JumpToHandlerException( handler, new YieldException(exn) );
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
    // Regular yield up to the handler we found
    _resume_tail_count = 0;
    return new Yield(handler,branch,op_arg);
  }
}


function _show_effect(effect_name, handler_tag) {
  return effect_name + (handler_tag ? "." + handler_tag.toString() : "");
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
  return new Handler("<skip>",null,null,[],undefined,skip,_handler_kind._Skip,0);
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
    throw new JumpToHandlerException( handler, new YieldException(exn) );
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

function _Finalize(result) {
  this.result = result;
}

// run finalizers of a resumption
function finalize(resume, after) {
  if (resume===_resume_tail_return) {
    // we are in a tail direct handler; return with a Finalize result which will later finalize up to the handler
    return new _Finalize(after);
  }
  else {
    // we are in a regular handler; resume with a finalize exception
    const fexn = new FinalizeException();
    _handle_catch(
      function() { return resume_throw(fexn,undefined /* local state */); },
      function(exn) { if (exn===fexn) return after(); else throw exn; },
      true // catch finalize exceptions too
    );
  }
}

// resume with an exception
function resume_throw(resume,exn,local) {
  return resume(new YieldException(exn), local);
}

// Resume in a tail-resumptive branch
function _skip_resume( handler, skipframe, cont, arg ) {
  _handlers_push(skipframe);
  var res;
  try {
    res = cont(arg);
  }
  catch(exn) {
    // an exception was raised; we unwind back to the handler and reraise the exception there.
    throw new JumpToHandlerException( handler, new YieldException(exn) );
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
  else if (res instanceof _Finalize) {
    throw new FinalizeToHandlerException(handler, res.result);
  }
  else if (res instanceof Yield) {
    // extend the continuation with the skip frame
    const cont = res.cont; // capture locally
    res.cont = function(arg) {
      return _skip_resume( handler, skipframe, cont, arg );
    }
    // and reyield
    return res;
  }
  else {
    // we returned normally without resuming, finalizeing, or yielding!
    // in this case we need to unwind back to the handler and return from there.
    throw new JumpToHandlerException(handler, res);
  }
}

// Finalize exceptions are not handled by `catch` but do run finalizers.
function FinalizeException( message ) {
  this.message = message || "Internal finalize exception";
  if ("captureStackTrace" in Error) {
    Error.captureStackTrace(this,FinalizeException);  // best on Node.js
  }
  else {
    this.stack = (new Error()).stack; // in browsers
  }
}
FinalizeException.prototype = Object.create(Error.prototype);
FinalizeException.prototype.constructor = FinalizeException;

function FinalizeToHandlerException( handler, result ) {
  FinalizeException.call(this,null);
  this.handler = handler;
  this.result = result;
}
FinalizeToHandlerException.prototype = Object.create(FinalizeException.prototype);
FinalizeToHandlerException.prototype.constructor = FinalizeToHandlerException;

// A special exception to `long jump` back to a handler without running finalizers
function JumpToHandlerException( handler, result ) {
  this.handler = handler;
  this.result = result;
  this.message = "Internal jump-to-handler exception";
  if ("captureStackTrace" in Error) {
    Error.captureStackTrace(this,JumpToHandlerException);  // best on Node.js
  }
  else {
    this.stack = (new Error()).stack; // in browsers
  }
}
JumpToHandlerException.prototype = Object.create(Error.prototype);
JumpToHandlerException.prototype.constructor = JumpToHandlerException;


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

function new_branch_scoped_once( op_tag, branch_fun ) {
  return new_branch(_resume_kind.ScopedOnce,op_tag,branch_fun);
}

function new_branch_scoped( op_tag, branch_fun ) {
  return new_branch(_resume_kind.Scoped,op_tag,branch_fun);
}



/*----------------------------------------------------------------
  Create a handler
----------------------------------------------------------------*/




var _handler_tags = 1;

function Handler(effect_name, reinit, finally_fun, branches, local, skip, handler_kind, handler_tag) {
  this.effect_name = effect_name;
  this.reinit      = reinit;
  this.finally_fun = finally_fun;
  this.branches    = branches;
  this.local       = local;
  this.skip        = (skip ? skip : 0);
  this.handler_kind= (handler_kind ? handler_kind : _handler_kind.Deep);
  this.handler_tag = (handler_tag ? handler_tag : _handler_tags++);
}

// Create a handler for a given effect

// Create a handler for a given effect
function _new_handlerx( effect_name, reinit, return_fun, finally_fun, branches0, handler_kind, handler_tag, wrap_handler_tag)
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
  const shared_handler = new Handler(effect_name, reinit, finally_fun, branches, undefined, 0, handler_kind, handler_tag);

  // create a special return operation branch to stay well-typed in `_handle_op`
  const return_branch = (return_fun==null ? null :
                           (finally_fun==null 
                             ? new_branch_never( null, function(resume,op_arg,local) { 
                                return return_fun(op_arg,local); 
                               }) 
                             : new_branch_never( null, function(resume,op_arg,local) {
                                 return bind( return_fun(op_arg,local), function(res) {
                                   return bind_const( finally_fun(local), res );
                                 });
                               }) 
                           ));

  // return a handler function: `action` is executed under the handler.
  return (function(action,local) {
	  // we only need to create fresh handlers for handlers with local state
    const handler = (local === undefined ? shared_handler :
                      new Handler(effect_name, reinit, finally_fun, branches, local, 0, handler_kind, handler_tag));
    
    // return clauses are treated as a special operation that is yielded
    const baction = (return_branch==null ? action : function(htag) {
                                                      return bind(action(htag), function(x) {
                                                        return new Yield(handler, return_branch, x);
                                                      });
                                                    });    
		// the handler action gets the resource argument (the identifier of the handler)
		const haction = function() {
                      const resource = (wrap_handler_tag==null ? handler.handler_tag : wrap_handler_tag(handler.handler_tag));
                      return yield_iter( baction( resource ) ); // autoconvert from iterators
                    }
    return _handle_action(handler, haction);
  });
}

function new_empty_handler( reinit, return_fun, finally_fun ) {
  return (function(action) { 
    var haction = (return_fun == null ? action : 
                   function() { return bind(action(), return_fun); });
    if (reinit==null && finally_fun==null) {
      return haction();
    }
    else {
      return handle_finally( haction, finally_fun, reinit );
    }
  });
}

function new_handler( effect_name, reinit, return_fun, finally_fun, branches, handler_kind ) {
  return _new_handlerx( effect_name, reinit, return_fun, finally_fun, branches, handler_kind, 0, null );
}

function new_handler_linear( effect_name, reinit, return_fun, finally_fun, branches ) {
  return new_handler(effect_name,reinit,return_fun,finally_fun,branches,_handler_kind.Linear);
}

function new_handler_shallow( effect_name, reinit, return_fun, finally_fun,   branches ) {
  return new_handler(effect_name,reinit,return_fun,finally_fun,branches,_handler_kind.Shallow);
}


function new_handler1( effect_name, reinit, return_fun, finally_fun, branches, handler_kind ) {
  const h = new_handler(effect_name, reinit, return_fun, finally_fun, branches, handler_kind);
  return (function(local,action) { return h(action,local); });
}
function new_handler1_linear( effect_name, reinit, return_fun, finally_fun, branches ) {
  return new_handler1(effect_name, reinit, return_fun, finally_fun, branches, _handler_kind.Linear);
}
function new_handler1_shallow( effect_name, reinit, return_fun, finally_fun, branches ) {
  return new_handler1(effect_name, reinit, return_fun, finally_fun, branches, _handler_kind.Shallow);
}
function new_resource_handler( effect_name, reinit, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag ) {
  return _new_handlerx( effect_name, reinit, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag );
}
function new_resource_handler1( effect_name, reinit, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag ) {
  const h = new_resource_handler(effect_name, reinit, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag );
  return (function(local,action) { return h(action,local); });
}


// Run action under a handler
function _handle_action( handler, action ) {
  return _handle_op( handler, _handle_cont(handler, action, undefined), [] );
}

// Execute under a handler
function _handle_cont(handler,cont,arg) {
  _handlers_push(handler); 
  let result;
  try {
    result = cont(arg);
    if (result instanceof Yield) {
      if (!(handler.handler_kind === _handler_kind.Shallow && handler === result.handler)) {
        // extend the continuation
        const cont = result.cont;
        result.cont = function(arg) {
          return _handle_cont( handler, cont, arg );
        };
      }
    }
    return result;
  }
  catch(exn) {
    // convert exceptions to results
    if (exn instanceof JumpToHandlerException && exn.handler === handler) {
      return exn.result;
    }
    else if (handler.finally_fun != null) {
      return new Yield( handler, null /* branch */, function() {
        return bind( handler.finally_fun(handler.local), function(_res) {
          if (exn instanceof FinalizeToHandlerException && exn.handler===handler) {
            return exn.result();  // argument to `finalize` is a function, run it now
          }
          else {
            return new YieldException(exn); // rethrow, possibly through todo's
          }
        });
      });
    }
    else if (exn instanceof FinalizeToHandlerException && exn.handler===handler) {
      return new Yield( handler, null /* branch */, exn.result ); // exn.result is the argument of `finalize`
    }
    else {
      return new YieldException(exn); // rethrow, possibly through todo's
    }
  }
  finally {
    _handlers_pop(handler);
  }
}



/*----------------------------------------------------------------
  The core handling function of operations
----------------------------------------------------------------*/

// Process the result of an action or resumption
function _handle_op(handler,result,todo_readonly) {
  let todo = todo_readonly;
  // the "trampoline" ensures that `resume` calls don't use extra stack space.
  // the `todo` array is an explicit list of continuations that still need to be done after a
  // resume returns. For example, `op(x) -> bind(resume(42),(i) => i + x)` would have `(i) => i + x` as a continuation.
  while(true) {
    if (result instanceof Yield)
    {
      if (result.handler !== handler) {
        // not handled by us: extend the continuation and break
        const cont = result.cont; // capture locally
        const local = handler.local;
        let resume_count = 0;
        result.cont = function(arg) {
          if (local !== undefined) handler.local = local;
          resume_count++;
          // if we resume more than once, reinit clauses can reinitialize the local state
          if (resume_count > 1 && handler.reinit != null) {
            return bind( handler.reinit(handler.local), function(newlocal) {
              if (newlocal !== undefined) handler.local = newlocal;
              return _handle_op( handler, cont(arg), todo);
            });
          }
          else {
            return _handle_op(handler,cont(arg),todo);
          }
        }
        break;
      }
      else if (result.branch===null) {
        // a resume is yielded as a special operation to trampoline them
        if (result.cont !== _cont_id) {  // happens with tail-resumptive operations
          if (todo===todo_readonly) todo = todo_readonly.slice();  // copy on write
          todo.push(result.cont);   // remember what to do after the resume
        }
        result = result.op_arg(); // the resume function is the operation argument
      }
      else {
        // handle the operation
        const cont = result.cont; // capture locally
        const branch = result.branch;
        let resume_fun;
        if (branch.resume_kind===_resume_kind.Never) {
          // avoid allocation for never resuming operations
          resume_fun = _resume_never;
        }
        else if (branch.resume_kind <= _resume_kind.Scoped && handler.handler_kind !== _handler_kind.Shallow) {
          // `resume` is yielded as a special operation; todo: allow this for shallow resumptions too?
          resume_fun = function(arg,local) {
                         return _yield_resume(handler,cont,arg,local);
                       };
        }
        else {
          // `resume` directly, will use more stack space since it calls `_handle_op` again
          resume_fun = function(arg,local) {
                         if (local !== undefined) handler.local = local;
                         return _handle_op(handler,cont(arg),todo);
                       };
        }
        result = branch.branch_fun( resume_fun, result.op_arg, handler.local );
      }
    }
    else if (todo.length > 0) {
      // still things to do..
      if (todo===todo_readonly) todo = todo_readonly.slice();  // copy on write
      const cont = todo.pop();
      result = cont(result);  // note: this will also propagate exceptions through todo`s
    }
    else if (result instanceof YieldException) {
      throw result.exn;  // rethrow
    }
    else {
      // final result
      break;
    }
  }
  return result;
}

// Yield a `resume` call as a special operation. This is done to
// execute resumptions in constant stack space but assumes that the body
// of each branch uses `bind` translation.
function _yield_resume( handler, cont, arg, local ) {
  function resume_cont() {
    // set the local state but don't use `handle_op` as we resume inside the trampoline of `handle_op`
    if (local !== undefined) handler.local = local;
    return cont(arg);
  }
  return new Yield(handler, null /*branch*/, resume_cont /*op_arg*/);
}

// Catch an operation branch resuming while promising to never resume
function _resume_never(arg,local) {
  throw new Error("A handler branch marked as never-resuming tried to resume.")
}

/*----------------------------------------------------------------
  Injection frames
----------------------------------------------------------------*/

function handle_inject( effect_name, handler_tag, action ) {
  const inject = new Handler(effect_name,null,null,[],undefined,0,_handler_kind._Inject,handler_tag);
  return _resume_inject( inject, action, undefined );
}

function _resume_inject( inject, cont, arg ) {
  _handlers_push(inject);
  try {
    const res = cont(arg);
    if (res instanceof Yield) {
      const cont = res.cont;
      res.cont = function(arg) {
        return _resume_inject(inject, cont, arg);
      }
    }
    return res;
  }
  finally {
    _handlers_pop(inject);
  }
}

/*----------------------------------------------------------------
  Extension: make it possible to catch exceptions over effectful
  computations; in particular, this scopes over the `next` continuations
  of the `bind`s.
----------------------------------------------------------------*/



function handle_catch( action, on_exn, catchall) {
  return _handle_catch_resume( action, undefined, on_exn, (catchall ? true : false));
}

function handle_finally( action, on_final, reinit) {
  return _handle_finally_resume( action, undefined, on_final, reinit);
}

function _handle_catch_resume( cont, arg, on_exn, catchall) {
  try {
    const result = cont(arg);
    if (result instanceof Yield) {
      const cont = result.cont; // capture locally
      result.cont = function(arg) {
        return _handle_catch_resume(cont, arg, on_exn, catchall);  // resume within the try block
      };
    }
    return result;
  }
  catch(exn) {
    if (exn instanceof JumpToHandlerException) {
      throw exn; // don't catch long jump exceptions
    }
    else if (!catchall && exn instanceof FinalizeException) {
      throw exn; // rethrow finalize exceptions
    }
    else if (exn._inject != null && exn._inject > 0) {
      // pass injected exception through
      exn._inject--;
      throw exn;
    }
    else {
      return on_exn(exn);
    }
  }
}

function _handle_finally_resume( cont, arg, on_final, reinit) {
  let result;
  try {
    result = cont(arg);
  }
  catch(exn) {
    if (exn instanceof JumpToHandlerException) {
      throw exn; // don't finalize on long jump exceptions
    }
    return bind(on_final(), function(_x) { throw exn; });
  }
  if (result instanceof Yield) {
    const cont = result.cont; // capture locally
    let resumes = 0;
    result.cont = function(arg) {
      resumes++;
      var c = cont;
      if (reinit != null && resumes > 1) {
        c = function(arg) {
              return bind(reinit(), function(_x) { return cont(arg); });
            };
      }
      return _handle_finally_resume(c, arg, on_final, reinit);  // resume within the try block
    };
    return result;
  }
  else {
    return bind(on_final(), function(_x) { return result; });
  }
}

function handle_inject_exn( action ) {
  return _resume_inject_exn(action,undefined);
}

function _resume_inject_exn( cont, arg ) {
  try {
    const result = cont(arg);
    if (result instanceof Yield) {
      const cont = result.cont; // capture locally
      result.cont = function(arg) {
        return _resume_inject_exn(cont, arg);  // resume within the try block
      };
    }
    return result;
  }
  catch(exn) {
    if (exn instanceof JumpToHandlerException) {
      throw exn; // don't catch long jump exceptions
    }
    else if (exn instanceof FinalizeException) {
      throw exn; // rethrow finalize exceptions
    }
    else {
      // increase injection level
      if (exn._inject == null) exn._inject = 1
      else exn._inject++;
      throw exn;  // rethrow
    }
  }
}

/*----------------------------------------------------------------
  Extension: functions to work with generator style yielding of operations.
----------------------------------------------------------------*/

// Transform the result of an effectful iterator (i.e. where each operation is `yield`ed) to regular operation yields.
function yield_iter( x ) {
  return (_is_iterator(x) ? _yield_iter_resume(x) : x);
}

// Is this an iterator?
function _is_iterator(x) {
  return (x != null && typeof x.next === "function");
}

// Handle operation yields that are yielded through an iterator.
function _yield_iter_resume( iter, arg ) {
  let res = (arg instanceof YieldException ? iter.throw(arg.exn) : iter.next(arg));
  if (!res.done) {
    const yld = res.value;
    _assert(yld instanceof Yield, "regular operations should be wrapped in `to_iter` to lift them to an iterator (function*)");
    if (yld.branch.resume_kind > _resume_kind.Once || yld.branch.resume_kind === _resume_kind.Scoped) return _yield_iter_not_once(yld);
    const cont = yld.cont;  // capture locally
    yld.cont = function(argval) {
      if (cont === _cont_id) {
        return _yield_iter_resume(iter,argval);
      }
      else {
        return bind(cont(argval), function(bindarg) {
          return _yield_iter_resume(iter,bindarg);
        });
      }
    };
  }
  return res.value;
}

function _yield_iter_not_once(yld) {
  throw new Error("Effects yielded in iterators should resume at most once! (" + yld.handler.effect_name + ")");
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
  yield_op      : yield_op,
  bind          : bind,
  new_branch    : new_branch,
  new_empty_handler : new_empty_handler,
  new_handler   : new_handler,
  new_handler1  : new_handler1,
  handle_catch  : handle_catch,
  handle_finally: handle_finally,
  new_resource_handler   : new_resource_handler,
  new_resource_handler1  : new_resource_handler1,

  handle_inject     : handle_inject,
  handle_inject_exn : handle_inject_exn,

  resume_throw  : resume_throw,
  finalize      : finalize,
  FinalizeException: FinalizeException,

  // support iterators
  to_iter     : to_iter,
  yield_iter  : yield_iter,

  // convenience
  new_handler_linear    : new_handler_linear,
  new_handler_shallow   : new_handler_shallow,
  new_handler1_linear   : new_handler1_linear,
  new_handler1_shallow  : new_handler1_shallow,

  new_branch_never      : new_branch_never,
  new_branch_tail       : new_branch_tail,
  new_branch_scoped_once: new_branch_scoped_once,
  new_branch_scoped     : new_branch_scoped,
  new_branch_once       : new_branch_once,
  new_branch_normal     : new_branch_normal,

  // unsafe debug/extension exports
  Yield      : Yield,
  _handlers  : _handlers,
  _enable_tail_resume_optimization: _enable_tail_resume_optimization,
};


})(); // Eff

// extend std_core with these primitives
$std_core = _export($std_core, {
  _yield_op     : Eff.yield_op,
  _bind         : Eff.bind,
  _new_branch   : Eff.new_branch,
  _new_branch1  : Eff.new_branch,
  _new_empty_handler: Eff.new_empty_handler,
  _new_handler  : Eff.new_handler,
  _new_handler1 : Eff.new_handler1,
  _new_resource_handler: Eff.new_resource_handler,
  _new_resource_handler1: Eff.new_resource_handler1,
  _handle_catch   : Eff.handle_catch,
  _handle_finally : Eff.handle_finally,
  _run_finalizers : Eff.finalize,
  _FinalizeException: Eff.FinalizeException,
  _handle_inject  : Eff.handle_inject,
  _handle_inject_exn: Eff.handle_inject_exn,
});
