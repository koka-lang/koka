/*---------------------------------------------------------------------------
  Copyright 2017 Daan Leijen, Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
General notes:

- _funtions starting with an underscore are meant to be private

General design:

* Bind and Yield:
  - To get results from effectful functions use `bind`. For example, `return bind( get(), function(i) {... })`
  - Effectful functions either return with their result, or they _yield_ an operation.
  - A yielded operation is an object of the form:
    ```
    { _yield    : bool    // special field to recognize yielding an operation versus a regular return 
      cont      : a -> b  // the continuation to resume with a result for the yielded operation (like `moveNext`)
      op_arg    : c       // the operation argument (like an integer for a `set` operation)
      handler   : handler // the handler that handles the effect
      branch    : branch  // the branch in the handler that handles this operation
    }
    ```
    where the operation has type, `c -> a`, and the action argument to the handler has a
    result type `b`.
  - A bind operation extends the continuation field `cont` to be able to resume correctly

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
  - For branches that are _tail resumptive_, ie. do a `resume` as their final tail call, we can 
    now optimize by calling the branch directly as direct function call: there is no need to
    yield explicity and unwind the stack and finally resume again.
  - But when optimizing such tail-resumptive call, we need to be careful that any operations called
    _inside_ the handler are only handled by handlers above it. For this to work we use _skip frames_.

* Tail-recursive effectful operations
  - Finally, for tail-recursive functions that use `bind`, we have a special `tailcall` yield that
    unwinds to the nearest bind and then initiates the tail call such that the stack space stays 
    constant. See `tailcall` and the `_tailcalling`.

* Skip frames
  - todo

* Exceptions
  - todo

---------------------------------------------------------------------------*/

const Eff = (function() {
  // The identity function
  function id(x) {
    return x;
  }

  // For assertions
  function _assert(b,msg) {
    if (!b) {
      msg = msg || "no message";
      throw new Error("Assertion failed: " + msg );
    }
  }


  // Different resume kinds for branches: used for potential optimizations (`Normal` is always correct to use).
  // Especially branches that `Tail` resume are very common and should be optimized well: in this library
  // these are resolved to direct method calls without needing to unwind/rewind the stack.
  const _resume_kind = {
    Default : 0,
    Never   : 1,    // never calls `resume`.
    Tail    : 2,    // calls `resume` at most once as the last thing to do (or does not resume at all).
    Once    : 3,    // calls `resume` at most once.
    Normal  : 4     // first-class resumption: calls `resume` once or many times.
  };


  /*----------------------------------------------------------------
    Bind, yield, and tailcall
  ----------------------------------------------------------------*/

  // Is this is a yielding value? Relies on the `_yield` field being special.
  function _yielding(x) {
    return (x != null && x._yield === true);
  }

  // Is this a tailcalling function? Optimization for effectful recursive functions to not use up the stack.
  function _tailcalling(x) {
    return (x != null && x._tailcall != null);
  }

  // Handle tailcall yields recursively
  function _handle_tailcalls(x) {
    while(_tailcalling(x)) { x = x._tailcall(); }
    return x;
  }

  // Bind a result of an effectful computation to the `next` function. 
  // When the effectful computation yielded on operation, we can remember where to continue (namely the `next` function)
  function bind( x, next ) 
  {
    x = _handle_tailcalls(x);
    if(_yielding(x)) {
      // an operation was yielded, re-yield with an updated resume continuation
      // note: we could perhaps maintain an array of `bind next` 
      //       functions instead of allocating separate closures.
      if (x.branch.resume_kind !== _resume_kind.Never) {
        const cont = x.cont; // capture locally
        x.cont = function(arg) { 
          return bind( cont(arg), next ); 
        };
      }
      // and return the updated yield value
      return x;
    }
    else {
      // not yielding, pass on the value to the next thing to do.
      return next(x);
    }
  }

  // Yield an operation consists of yielding a return object.
  function yield_op( effect_tag, op_tag, op_arg ) {
    // find our handler in the global handler stack
    const r = _find_handler(effect_tag,op_tag);
    if (r.branch.resume_kind !== _resume_kind.Tail) {
      // The handler uses a regular resumption, start unwinding the through the binds upto the handler
      return { 
        _yield    : true,               // signify we are yielding
        op_arg    : op_arg,             // the operation arguments (the new state)
        cont      : id,                 // the resume continuation: called with the operation result,
        handler   : r.handler,          // to handler to where to yield
        branch    : r.branch,           // the handler branch that matches our operation      
      }
    }
    else {
      // The handler uses a tail resumption: optimize it by directly 
      // calling the branch function without any unwinding.
      // This is important in practice as 90% of all operation handlers are tail-resumptions.
      const skiphandler = { effect_tag: _effect_skip, skip: r.skip, handler: r.handler };  // todo: avoid this allocation with special skip frames?
      return _skip_handle(skiphandler, r.branch.branch_fun, op_arg );
    }
  }

  // Yield a tailcall; use this for tail-recursive effectful computations to not use unneeded stack.
  function tailcall( action ) {
    return {
      _yield   : true,
      _tailcall: action
    };
  }


  /*----------------------------------------------------------------
    The handler stack
  ----------------------------------------------------------------*/

  // The global handler stack
  const handlers = [];

  function _handlers_push(h) {
    handlers.push(h);
  }

  function _handlers_pop(expected) {
    const h = handlers.pop();
    _assert(h === expected, "Handler stack is out of sync!");
  }

  // Find the handler for a particular effect and operation.
  function _find_handler( effect_tag, op_tag ) {
    // find the handler that handles the given effect
    var skipping = 0;
    for(var i = handlers.length - 1; i >= 0; i--) {
      let h = handlers[i];      
      if (skipping > 0) {
        // skip when needed due to a skip frame
        skipping--;
      }
      else if (h.effect_tag == effect_tag) {
        // we found a handler for our effect
        for(var j = 0; j < h.branches.length; j++) {
          // find a matching branch for the operation
          let b = h.branches[j];
          if (b.op_tag == op_tag) {
            return { handler: h, branch: b, skip: handlers.length - i };
          }
        }
        throw new Error("Bad handler: Operation " + op_tag + " is not found in the handler for " + effect_tag);
      } 
      else if (h.effect_tag == _effect_skip) {
        // on skip frames, we start skipping
        skipping = h.skip;
      }
    }
    throw new Error("There is no handler for " + effect_tag + "/" + op_tag);
  }



  /* ----------------------------------------------------------------
    Skip frames: skip on a first read :-)

    When doing direct calls to tail-resumptive branches, we push
    a skip frame to ensure that any operations in a handler are not handled
    by any operations under it in the handler stack.

    There are other approaches, like popping the handler stack,
    but this seems most efficient?
  ----------------------------------------------------------------*/

  // Special effect tag of skip handlers
  const _effect_skip = "<skip>";

  // The tail resume just returns the results in a record.
  function _tail_resume(x,local) {
    return { _result: x, local: local };  // we assume `_result` is a special field
  }

  // Call a tail-resumptive branch using a skip frame handler
  function _skip_handle( skiphandler, branch_fun, op_arg) {
    _handlers_push(skiphandler);
    const x = _handle_tailcalls( branch_fun(_tail_resume, op_arg, skiphandler.handler.local) );  
    _handlers_pop(skiphandler);
    return _skip_handle_yield( skiphandler, x );
  }

  // Resume in a tail-resumptive branch 
  function _skip_resume( skiphandler, cont, arg ) {
    _handlers_push(skiphandler);
    const x = _handle_tailcalls( cont(arg) );
    _handlers_pop(skiphandler);
    return _skip_handle_yield( skiphandler, x );
  }

  // Handle the result of a tail-resumptive branch 
  function _skip_handle_yield( skiphandler, x ) {
    if (_yielding(x)) {
      // extend the continuation with the skip frame 
      _assert(!_tailcalling(x), "_skip_handle_yield: tail calls should already have been handled.");
      if (x.branch.resume_kind !== _resume_kind.Never) {
        const cont = x.cont; // capture locally
        x.cont = function(arg) {
          return _skip_resume( skiphandler, cont, arg );
        }
      }
      // and reyield
      return x;
    }
    else if (x != null && x._result !== undefined) {
      // we tail-resumed
      // record the new local state of the handler in-place
      if (x.local !== undefined) {
        skiphandler.handler.local = x.local;
      }
      return x._result;
    }
    else {
      // we returned normally without resuming!
      // in this case we need to unwind back to the handler anyway and return from there.
      // yield specially with a `_tail_return_branch` to signify this.
      return {
        _yield: true,
        op_arg: x,
        cont: id,
        handler: skiphandler.handler,
        branch: _tail_return_branch
      };
    }
  }

  // The special tail return branch is a default branch that always returns its argument directly.
  const _tail_return_branch = {
    op_tag: "<tail return>",
    resume_kind: _resume_kind.Never,
    branch_fun: function(resume,op_arg,local) {
      return op_arg;
    }
  };


  /*----------------------------------------------------------------
    Create an operation branch; i.e. one match in a handler
  ----------------------------------------------------------------*/

  // Create an handler branch, i.e. the function that is called for a particular operation
  function new_branch( resume_kind, op_tag, branch_fun ) {
    return {
      op_tag: op_tag,
      resume_kind: resume_kind || _resume_kind.Normal,
      branch_fun: branch_fun,
    };
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



  /*----------------------------------------------------------------
    Create a handler
  ----------------------------------------------------------------*/

  // Create a handler for a given effect
  function new_handler( effect_tag, return_fun, branches ) 
  {
    branches = branches || [];
    return_fun = return_fun || id;

    // shared handler if there is no local state
    const shared_handler = {
      effect_tag: effect_tag,
      branches: branches
    };
    
    // handle an action or resumption
    function handle(action,local,arg) {
      // only share handler record if no local state is needed; otherwise we need to recreate the handler
      // to ensure with tail call optimizations the local state is not shared among different handler instantiations.
      const h = (local === undefined ? shared_handler : { effect_tag: effect_tag, branches: branches, local: local });
      _handlers_push(h);
      const x = _handle_tailcalls( action(arg) );
      _handlers_pop(h);
      return handle_yield(x, h, h.local);
    }

    // process the result of an action or resumption
    function handle_yield(x, h, local) {
      if (_yielding(x)) {
        _assert(!_tailcalling(x), "handle_yield: tail calls should already have been handled");
        // are we yielding to this handler?
        if (x.handler !== h) {
          // no, update the continuation
          if (x.branch.resume_kind !== _resume_kind.Never) {
            const cont = x.cont;         // capture local continuation
            x.cont = function(arg) { 
              return handle(cont,local,arg); 
            };          
          }
          // and re-yield
          return x;
        }
        else {
          h = null;           // for gc
          x.handler = null;   // for gc
          // handle the operation
          // note: no need to optimize tail resumptions, already handled in `yield`
          if (x.branch.resume_kind !== _resume_kind.Never) {
            const cont = x.cont;                  // capture local continuation
            function resume(result,newlocal) {    // create a resuming function
              return handle(cont, (newlocal!==undefined ? newlocal : local), result);  
            }
            return x.branch.branch_fun( resume, x.op_arg, local );          
          }
          else {
            return x.branch.branch_fun( _never_resume, x.op_arg, local ); 
          }       
        }
      }
      else {
        // a regular result without yielding
        return return_fun(x, local);
      }
    }

    // return a handler function
    return (function(action,local) { 
      return handle(action,local,undefined); 
    });
  }

  function new_handler1( effect_tag, return_fun, branches ) {
    function return_fun_swap(result,local) {
      return return_fun(local,result);
    }
    const f = new_handler(effect_tag,return_fun_swap,branches);
    return (function(local,action) { return f(action,local); });
  }

  function new_branch1( resume_kind, op_tag, branch_fun ) {
    function branch_fun_swap( resume, op_arg, local ) {
      return branch_fun( function(newlocal,result) { 
        return resume(result,newlocal); 
      }, op_arg, local );
    }
    return new_branch(resume_kind,op_tag,branch_fun_swap);
  }

  function _never_resume() {
    throw new Error("A handler branch marked as never resuming tried to resume.")
  }


  /*----------------------------------------------------------------
    Extension: functions to work with generator style yielding of operations.
  ----------------------------------------------------------------*/

  // Transform the result of an effectful iterator to regular operation yields.
  function yield_iter( x ) {
    return (_is_iterator(x) ? _yield_iter_resume(x,undefined) : x);
  }

  // Is this an iterator?
  function _is_iterator(x) {
    return (x != null && typeof x.next === "function");
  }

  // Handle operation yields that are yielded through an iterator.
  function _yield_iter_resume( iter, arg ) {
    let res = iter.next(arg);
    const x = res.value;
    if (!res.done) {
      _assert(_yielding(x), "regular operations should be wrapped in `to_iter` to lift them to an iterator (function*)");
      _assert(x.branch.resume_kind < _resume_kind.Normal, "effects yielded in iterators should resume at most once!");
      // _assert(x.cont === id, "effects yielded inside an iterator should not be bound using `bind`.");
      const cont = x.cont;  // capture locally 
      x.cont = function(arg) {
        return bind(cont(arg), function(argval) {  // bind in case of yielding over a regular yielding computation
          return _yield_iter_resume(iter,argval);
        });
      };
    }
    return x;
  }

  // Use this instead of a `yield` keyword to optimize for tail-resumptive operations that might
  // return a value right away.
  // Note: perhaps we should implement a `next` method on the yield object so we can use the `yield*` keyword?
  function* to_iter( x ) {
    if (_yielding(x)) {
      return yield x;
    }
    else {
      return x;
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
  function handle_exn( action, on_exn ) {
    return _handle_exn_resume( action, on_exn, undefined);
  }

  function _handle_exn_resume( action, on_exn, arg) {
    const n = handlers.length; // remember the handler stack length so we can restore it on an exception
    try {
      const x = _handle_tailcalls(action(arg));
      _assert(handlers.length === n, "Handler stack is out of sync in a try block");
      if (_yielding(x)) {
        let cont = x.cont; // capture locally
        x.cont = function(arg) {
          return _handle_exn_resume(cont, on_exn, arg);  // again execute within the try block
        };
      }   
      return x;
    }
    catch(exn) {
      _assert(handlers.length >= n, "After exception the handlers stack is too shallow");
      if (handlers.length > n) {
        handlers.length = n; // restore handler stack 
      }
      return on_exn(exn);
    }
  }

  let _in_bind_context = false;
  function is_in_bind_context() {
    return _in_bind_context;
  }

  function set_in_bind_context(x) {
    _in_bind_context = (x ? true : false);
  }

  return {
    yieldop       : yield_op,
    bind          : bind,
    new_branch    : new_branch,
    new_branch1   : new_branch1,
    new_handler   : new_handler,
    new_handler1  : new_handler1,
    handle_exn    : handle_exn,
    to_iter       : to_iter,
    yield_iter    : yield_iter,
    is_in_bind_context : is_in_bind_context,
    set_in_bind_context: set_in_bind_context,
  }
})();


// extend std_core with these primitives
$std_core = _export($std_core, {
  _yieldop      : Eff.yieldop,
  _bind         : Eff.bind,
  _new_branch   : Eff.new_branch,
  _new_branch1  : Eff.new_branch1,
  _new_handler  : Eff.new_handler,
  _new_handler1 : Eff.new_handler1,
  _handle_exn   : Eff.handle_exn,
  _is_in_bind_context : Eff.is_in_bind_context,
  _set_in_bind_context: Eff.set_in_bind_context,
});


