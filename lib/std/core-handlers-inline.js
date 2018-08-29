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
function __extends(d, b) {
  for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
  function __() { this.constructor = d; }
  __.prototype = b.prototype;
  d.prototype = new __();
};

var Eff = (function() {
/*---------------------------------------------------------------------------
  Copyright 2017 Daan Leijen & Manuel Serrano

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

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
// Branches that are
const _resume_kind = {
  Never      : 0,    // never calls `resume`.
  Tail       : 1,    // calls `resume` at most once as the last thing to do (or does not resume at all). (and the handler is not `Shallow`)
  ScopedOnce : 2,    // at most once, and in the scope of the branch
  Scoped     : 3,    // calls `resume` in the scope of the branch
  Once       : 4,    // calls `resume` at most once, perhaps outside the scope of the branch.
  Normal     : 5,    // first-class resumption: calls `resume` once or many times
  OnceRaw    : 6,    // variants of Once and Normal that do not implicitly finalize()
  NormalRaw  : 7,
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
  // these fields are used to efficiently communicate values in `_handle_result`
  this.local   = undefined;
  this.match   = false;
}

function ExceptionValue(exn) {
  this.exn = exn;
}

function _cont_id( arg ) {
  if (arg instanceof ExceptionValue) throw arg.exn;
  return arg;
}

/*----------------------------------------------------------------
  Operation branches
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
  Handlers
----------------------------------------------------------------*/

var _handler_tags = 1;

function HandlerInfo(effect_name, reinit_fun, return_fun, finally_fun,
                     branches, skip, handler_kind, handler_tag)
{
  this.effect_name = effect_name;
  this.handler_tag = (handler_tag ? handler_tag : _handler_tags++);
  this.handler_kind= (handler_kind ? handler_kind : _handler_kind.Deep);
  this.skip        = (skip ? skip : 0);
  this.branches    = branches;
  this.reinit_fun  = reinit_fun;
	this.return_fun  = return_fun;
  this.finally_fun = finally_fun;
}

// Note: even for handlers without local state we need to allocate
// a `Handler` as these are also used to identify a handler on the stack
// uniquely.
function Handler( hinfo, local ) {
  this.info = hinfo;
  this.local = local;
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


/*----------------------------------------------------------------
  The handler stack
----------------------------------------------------------------*/

// The global handler stack
const _handlers = [];

function _handlers_push_hinfo( hinfo, local ) {
  return _handlers_push( new Handler(hinfo,local) );
}

// Note: use carefully as regular handlers are assumed to be unique on the stack
// This is only used to by skip- and inject frames.
function _handlers_push(h) {
  _handlers.push(h);
  return h;
}

function _handlers_pop(expected) {
  const h = _handlers.pop();
  _assert(h === expected, "Handler stack is out of sync!");
  return h.local;
}

function _handlers_top(idx_from_top) {
  return _handlers[_handlers.length - 1 - idx_from_top];
}


/*----------------------------------------------------------------
  Cancel Exceptions
----------------------------------------------------------------*/

// System exceptions are not handled by `catch` but do run finalizers.
const SystemException = (function() {
  __extends(SystemException,Error);
  function SystemException( message ) {
    this.message = message || "Internal system exception";
    if ("captureStackTrace" in Error) {
      Error.captureStackTrace(this,SystemException);  // best on Node.js
    }
    else {
      this.stack = (new Error()).stack; // in browsers
    }
  }
  return SystemException;
})();

const FinalizeException = (function() {
  __extends(FinalizeException,SystemException);
  function FinalizeException( handler, value ) {
    SystemException.call(this,"Internal finalize exception");
    this.handler = handler;
    this._value  = value;
  }
  FinalizeException.prototype.get_value = function() {
    if (this._value instanceof ExceptionValue) throw this._value.exn;
    return this._value;
  }
  FinalizeException.prototype.get_message = function() {
    const hinfo = this.handler.info;
    return this.message + " (to " + _show_effect(hinfo.effect_name,hinfo.handler_tag) + ")";
  }
  return FinalizeException;
})();



/*----------------------------------------------------------------
  Yield
----------------------------------------------------------------*/

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
    const hinfo = handler.info;
    if (hinfo.effect_name === effect_name &&
        (handler_tag === 0 || hinfo.handler_tag === handler_tag))
    {
      // we match on the handler effect + handler_tag
      if (hinfo.handler_kind===_handler_kind._Inject) inject++;   // inject effect
      else if (inject > 0) inject--;     // skip over this handler due to previous injection
      else return i;
    }
    i -= hinfo.skip; // skip over skip frames
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
  const hinfo  = handler.info;
  const skip   = _handlers.length - i;
  const branch = hinfo.branches[op_tag];
  if (branch==null) throw new Error("bad handler: operation " + op_name + " is not found in the handler for " + _show_effect(effect_name,handler_tag));

  const is_linear = (hinfo.handler_kind === _handler_kind.Linear);
  if (is_linear || (branch.resume_kind <= _resume_kind.Tail && _resume_tail_count < _max_resume_tails)) {
    if (!is_linear) _resume_tail_count++;
    // The handler uses a tail resumption: optimize it by directly
    // calling the branch function without any unwinding.
    // This is important in practice as 95% of all operation handlers are tail-resumptions.
    //return _call_tail_branch(handler,branch,skip,op_arg);  // Inline for performance
    const skipframe = _cached_skip_frame(skip);
    _handlers_push(skipframe);
    let res;
    try {
      res = branch.branch_fun( _context_skip_tail, op_arg, handler.local );
    }
    catch(exn) {
      // an exception was raised; this causes the implicit finalize to be
      // executed (section 8.5) and thus we continue here with finalization
      if (exn instanceof FinalizeException) {
        throw exn;
      }
      else {
        throw new FinalizeException( handler, new ExceptionValue(exn) );
      }
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

function _show_handler(hinfo) {
  return _show_effect(hinfo.effect_name,hinfo.handler_tag);
}

function _show_operation(hinfo,branch) {
  return _show_handler(hinfo) + "." + branch.op_tag.toString();
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
  return new Handler( new HandlerInfo( "<skip>",null,null,null,[],skip,_handler_kind._Skip,0) );
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


// Resume in a tail-resumptive branch
function _skip_resume( handler, skipframe, cont, arg ) {
  _handlers_push(skipframe);
  var res;
  try {
    res = cont(arg);
  }
  catch(exn) {
    // an exception was raised; this causes the implicit finalize to be
    // executed (section 8.5) and thus we continue here with finalization
    if (exn instanceof FinalizeException) {
      throw exn;
    }
    else {
      throw new FinalizeException( handler, new ExceptionValue(exn) );
    }
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
    const cont = res.cont; // capture locally
    res.cont = function(arg) {
      return _skip_resume( handler, skipframe, cont, arg );
    }
    // and reyield
    return res;
  }
  else {
    // we returned normally without resuming, or yielding!
    // in this case we need auto finalize back to our handler
    // (because we assume an implicit finalize around every (tail resumptive) branch)
    throw new FinalizeException(handler, res);
  }
}


/*----------------------------------------------------------------
  Create a handler
----------------------------------------------------------------*/

// Create a handler for a given effect
function _new_handlerx( effect_name, reinit_fun, return_fun, finally_fun,
                        branches0, handler_kind, handler_tag, wrap_handler_tag)
{
  // initialize the branches such that we can index by `op_tag`
  // regardless if the `op_tag` is a number or string.
  const branches = new Array(branches0.length);
  for(let i = 0; i < branches0.length; i++) {
    const branch = branches0[i];
    branches[branch.op_tag] = branch;
    if (typeof op_tag !== "number") branches[i] = branch;
  }

  var shared_hinfo = null;

  // return a handler function: `action` is executed under the handler.
  return (function(action,local) {
    // only resources need to recreate the `hinfo` due to the handlertag
    // todo: also store the handler tag in the Handler instead of the Info
    var hinfo = shared_hinfo;
    if (hinfo==null) {
      hinfo = new HandlerInfo(effect_name, reinit_fun, return_fun, finally_fun,
                                     branches, 0, handler_kind, handler_tag);
    }
    if (wrap_handler_tag==null && shared_hinfo==null) {
      shared_hinfo = hinfo;
    }

	  // the handler action gets the resource argument (the identifier of the handler)
		const haction = function() {
                      const resource = (wrap_handler_tag==null ? hinfo.handler_tag :
                                                                 wrap_handler_tag(hinfo.handler_tag));
                      return yield_iter( action( resource ) ); // autoconvert from iterators
                    };
    return _handle_action(hinfo, local, haction );
  });
}

function new_empty_handler( reinit_fun, return_fun, finally_fun ) {
  return (function(action) {
    var haction = (return_fun == null ? action :
                   function() { return bind(action(), return_fun); });
    if (reinit_fun==null && finally_fun==null) {
      return haction();
    }
    else {
      return handle_finally( haction, finally_fun, reinit_fun );
    }
  });
}

function new_empty_handler1( reinit_fun, return_fun, finally_fun ) {
  return (function(local,action) {
    var haction = (return_fun == null ? action :
                   function() { return bind(action(), function(x) {
                                  return return_fun(x,local) }
                                );
                              });
    if (reinit_fun==null && finally_fun==null) {
      return haction();
    }
    else {
      return handle_finally( haction,
        function(){ return finally_fun(local) },
        function(){ local = reinit_fun(local); }
      );
    }
  });
}

function new_handler( effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind ) {
  return _new_handlerx( effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind, 0, null );
}

function new_handler_linear( effect_name, reinit_fun, return_fun, finally_fun, branches ) {
  return new_handler(effect_name,reinit_fun,return_fun,finally_fun,branches,_handler_kind.Linear);
}

function new_handler_shallow( effect_name, reinit_fun, return_fun, finally_fun,   branches ) {
  return new_handler(effect_name,reinit_fun,return_fun,finally_fun,branches,_handler_kind.Shallow);
}


function new_handler1( effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind ) {
  const h = new_handler(effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind);
  return (function(local,action) { return h(action,local); });
}
function new_handler1_linear( effect_name, reinit_fun, return_fun, finally_fun, branches ) {
  return new_handler1(effect_name, reinit_fun, return_fun, finally_fun, branches, _handler_kind.Linear);
}
function new_handler1_shallow( effect_name, reinit_fun, return_fun, finally_fun, branches ) {
  return new_handler1(effect_name, reinit_fun, return_fun, finally_fun, branches, _handler_kind.Shallow);
}
function new_resource_handler( effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag ) {
  return _new_handlerx( effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag );
}
function new_resource_handler1( effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag ) {
  const h = new_resource_handler(effect_name, reinit_fun, return_fun, finally_fun, branches, handler_kind, handler_tag, wrap_handler_tag );
  return (function(local,action) { return h(action,local); });
}


/*----------------------------------------------------------------
  Handler invokation
----------------------------------------------------------------*/

// Run action under a handler
function _handle_action( hinfo, local, action ) {
  return _handle_resume( hinfo, local, action, undefined );
}

function _handle_resume( hinfo, local, cont, arg ) {
  const handler = _handlers_push_hinfo(hinfo,local);
  return _handle_result( hinfo, _handle_cont_pushed(handler, cont, arg) );
}

function _handle_resume_pushed( handler, cont, arg ) {
  return _handle_result( handler.info, _handle_cont_pushed(handler, cont, arg) );
}

function _handle_cont( hinfo, local, cont, arg ) {
  const handler = _handlers_push_hinfo(hinfo,local);
  return _handle_cont_pushed( handler, cont, arg);
}

// Execute under a handler
function _handle_cont_pushed(handler,cont,arg) {
  const hinfo = handler.info;
  var result;
  var local;
  try {
    result = cont(arg);
    local = _handlers_pop(handler);
  }
  catch(exn) {
    // rule (unwind), fig 9.
		local = _handlers_pop(handler);  // pop handler before calling finally
    const finally_fun = (hinfo.finally_fun || _cont_id);
		return bind( finally_fun(local), function(_res) {
      if (exn instanceof FinalizeException && exn.handler === handler) {
        return exn.get_value();  // might throw
      }
      else {
        throw exn;
      }
  	});
  }

  if (result instanceof Yield) {
    if (result.handler===handler) {
      // optimize: communicate local and handler match through the yield result..
      result.local = local;
      result.match = true;
    }
    else {
      // not handled by us: extend the continuation to resume under our handler.
      const cont  = result.cont; // capture locally
      let resume_count = 0;
      result.cont = function(arg) {
        resume_count++;
        // if we resume more than once, reinit_fun clauses can reinitialize the local state
        if (resume_count > 1 && hinfo.reinit_fun != null) {
          return bind( hinfo.reinit_fun(local), function(newlocal) {
            return _handle_resume( hinfo, newlocal, cont, arg );
          });
        }
        else {
          return _handle_resume( hinfo, local, cont, arg  );
        }
      }
    }
    return result;
  }
  // final result, execute return and finally clause
  // rule (return), fig 5.
  else if (hinfo.finally_fun != null) {
    return _bind_finally( hinfo, local,
                          function(_res){
                            return (hinfo.return_fun==null ? result : hinfo.return_fun(result,local));
                          }, result );
  }
  else if (hinfo.return_fun != null) {
    return hinfo.return_fun(result,local);
  }
  else return result;
}


/*----------------------------------------------------------------
  The core handling function of operations
----------------------------------------------------------------*/

// Process the result of handling an actions
function _handle_result(hinfo,result) {
  // the "trampoline" ensures that `resume` calls don't use extra stack space.
  while(result instanceof Yield && result.match) {
     // handle the operation
    const branch = result.branch;
    let context;
    if (branch.resume_kind===_resume_kind.Never) {
      context = _context_never;
    }
    else if (branch.resume_kind <= _resume_kind.Scoped && hinfo.handler_kind !== _handler_kind.Shallow) {
      // `resume` is yielded as a special operation; todo: allow this for shallow resumptions too?
      context = new ContextTail(hinfo,branch,result.cont);
    }
    else {
      // `resume` directly, will use more stack space since it calls `_handle_op` again
      context = new ContextNormal(hinfo,branch,result.cont);
    }
    result = _branch_invoke( branch, context, result.op_arg, result.local);
  }
  return result;
}

function _bind_finally( hinfo, local, f, x ) {
  // section 8.4: return as an operation
	var result;
	try {
		result = f(x);
		if (result instanceof Yield) {
			const cont = result.cont;
			result.cont = function(arg) {
				return _bind_finally( hinfo, local, cont, arg );
			}
			return result;
		}
	}
	catch(exn) {
		return bind( hinfo.finally_fun(local), function(_res) {
			throw exn;
		});
	}
	return bind_const(hinfo.finally_fun(local), result );
}

function _branch_invoke( branch, context, arg, local ) {
  // rule (handle_d), fig 10.
	return _protect( context, function(_arg) {
    if (branch.resume_kind >= _resume_kind.OnceRaw) {
      context.suppress_auto_finalize();
    }
    return branch.branch_fun( context, arg, local );
	}, null);
}

// This implements a `protect` frame around a branch,
// ensuring that the finalizer is always called implicitly even in
// the case of exceptions. Due to deep handlers, this will also
// invoke the finally clause of the handler of the branch itself.
// rule (protect) and (unprotect), fig 10.
// note: in contract to the rules, we also use implicit finalize around
// every branch unless auto finalization is suppressed.
function _protect( context, cont, arg ) {
	try {
		var result = cont(arg);
		if (result instanceof Yield) {
			if (!context.invoked) {
				// only extend the continuation the context is not already invoked
        // (through resume or finalize).
				const cont = result.cont;
				result.cont = function(arg) {
					return _protect( context, cont, arg );
				}
			}
			return result;
		}
  }
	catch(exn) {
		if (context.invoked) throw exn; // optimize to maintain a better exception stack
    return context.finalize( new ExceptionValue(exn) );  // rule (protect)
  }
	return context.auto_finalize(result);  // rule (unprotect) + implicit finalize
}

/*----------------------------------------------------------------
  Resumption Contexts
----------------------------------------------------------------*/

const Context = (function() {
  function Context() {
    this.invoked = false;
    this._suppress = false;
    this.auto_finalized = false;
  }

  Context.prototype.suppress_auto_finalize = function() {
    this._suppress = true;
  }

  Context.prototype.auto_finalize = function(arg) {
    if (this._suppress || this.invoked) return arg;
    this.auto_finalized = true;
    return this.finalize(arg);
  }

  Context.prototype.finalize = function(arg) {
    if (this.invoked) return arg;
    this.invoked = true;
    return this._resume_cancel( arg );
  }

  return Context;
})();


const ContextNever = (function() {
  __extends(ContextNever,Context);
  function ContextNever() {
    Context.call(this);
  }
  ContextNever.prototype.resume = function(arg,local) {
    throw new Error("A handler branch marked as never-resuming tried to resume.");
  }
  ContextNever.prototype._resume_cancel = function(arg) {
    throw new Error("A handler branch marked as never-resuming tried to finalize.");
  }
  return ContextNever;
})();

const _context_never = new ContextNever();


const ContextTail = (function() {
  __extends(ContextTail,Context);
  function ContextTail(hinfo,_branch,cont) {
    Context.call(this);
    this.hinfo = hinfo;
    this.cont = cont;
  }

  // A tail-resume uses a bare _handle_cont call since we expect
  // to return in the trampoline of _handle_result and continue
  // in that while loop
  ContextTail.prototype.resume = function(arg,local) {
    this.invoked = true;
    return _handle_cont(this.hinfo,local,this.cont,arg);
  }
  ContextTail.prototype._resume_cancel = function(arg) {
    this.invoked = true;
    const handler = _handlers_push_hinfo(this.hinfo,undefined);
    return _handle_cont_pushed(handler, this.cont, new ExceptionValue( new FinalizeException(handler,arg) ));
  }
  return ContextTail;
})();


const ContextNormal = (function() {
  __extends(ContextNormal,Context);
  function ContextNormal(hinfo,branch,cont) {
    Context.call(this);
    this.hinfo = hinfo;
    this.branch = branch;
    this.cont = cont;
  }
  ContextNormal.prototype.resume = function(arg,local) {
    // perhaps enable this as a warning in debug mode?
    /*
    if (this.auto_finalized) {
      throw new Error("Trying to resume an already auto-finalized resumption: " +
                       _show_operation(this.hinfo,this.branch) +
                       "\n  (hint: use a 'fun raw' operation clause instead)");
    }
    */
    this.invoked = true;
    return _handle_resume( this.hinfo, local, this.cont, arg);
  }
  ContextNormal.prototype._resume_cancel = function(arg) {
    this.invoked = true;
    const handler = _handlers_push_hinfo(this.hinfo, undefined /* local */);
    return _handle_resume_pushed( handler, this.cont,
                                  new ExceptionValue( new FinalizeException(handler,arg) ));
  }
  return ContextNormal;
})();


function _TailResume(result,local) {
  this.result = result;
  this.local = local;
}

const ContextSkipTail = (function(){
  __extends(ContextSkipTail,Context);
  function ContextSkipTail() {
    Context.call(this);
  }
  ContextSkipTail.prototype.resume = function(arg,local) {
    return new _TailResume(arg,local);
  }
  ContextSkipTail.prototype._resume_cancel = function(arg) {
    throw new Error("Branch marked as tail-resumptive tried to call finalize");
  }
  return ContextSkipTail;
})();
const _context_skip_tail = new ContextSkipTail();



/*----------------------------------------------------------------
  Injection frames
----------------------------------------------------------------*/

function handle_inject( effect_name, handler_tag, action ) {
  const inject = new Handler( new HandlerInfo(effect_name,null,null,null,[],0,_handler_kind._Inject,handler_tag) );
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
  catch(exn) {
    // rule (cancel_i), fig 9.
    if (exn instanceof FinalizeException && exn.handler===inject) {
      // wrap the exn inside another FinalizeException to keep propagating beyond the inject
      throw new FinalizeException(inject, new ExceptionValue(exn));
    }
    else {
      throw exn; // pass-through
    }
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

function handle_finally( action, on_final, reinit_fun) {
  return _handle_finally_resume( action, undefined, on_final, reinit_fun);
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
    if (!catchall && exn instanceof SystemException) {
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

function _handle_finally_resume( cont, arg, on_final, reinit_fun) {
  let result;
  try {
    result = cont(arg);
  }
  catch(exn) {
    return bind(on_final(), function(_x) { throw exn; });
  }
  if (result instanceof Yield) {
    const cont = result.cont; // capture locally
    let resumes = 0;
    result.cont = function(arg) {
      resumes++;
      var c = cont;
      if (reinit_fun != null && resumes > 1) {
        c = function(arg) {
              return bind(reinit_fun(), function(_x) { return cont(arg); });
            };
      }
      return _handle_finally_resume(c, arg, on_final, reinit_fun);  // resume within the try block
    };
    return result;
  }
  else {
    return bind_const(on_final(), result);
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
    if (exn instanceof SystemException) {
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
  let res = (arg instanceof ExceptionValue ? iter.throw(arg.exn) : iter.next(arg));
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
/*
function* to_iter( x ) {
  if (x instanceof Yield) {
    return yield x;
  }
  else {
    return x;
  }
}
*/

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
  new_empty_handler1 : new_empty_handler1,
  new_handler   : new_handler,
  new_handler1  : new_handler1,
  handle_catch  : handle_catch,
  handle_finally: handle_finally,
  new_resource_handler   : new_resource_handler,
  new_resource_handler1  : new_resource_handler1,

  handle_inject     : handle_inject,
  handle_inject_exn : handle_inject_exn,

  SystemException: SystemException,
  FinalizeException: FinalizeException,

  // support iterators
  // to_iter     : to_iter,
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
  __extends     : __extends,
  _yield_op     : Eff.yield_op,
  _bind         : Eff.bind,
  _new_branch   : Eff.new_branch,
  _new_branch1  : Eff.new_branch,
  _new_empty_handler: Eff.new_empty_handler,
  _new_empty_handler1: Eff.new_empty_handler1,
  _new_handler  : Eff.new_handler,
  _new_handler1 : Eff.new_handler1,
  _new_resource_handler: Eff.new_resource_handler,
  _new_resource_handler1: Eff.new_resource_handler1,
  _handle_catch   : Eff.handle_catch,
  _handle_finally : Eff.handle_finally,
  _SystemException: Eff.SystemException,
  _FinalizeException: Eff.FinalizeException,
  _handle_inject  : Eff.handle_inject,
  _handle_inject_exn: Eff.handle_inject_exn,
});
