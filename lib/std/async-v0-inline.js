/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var effCancel = "std/async/cancelable";

var _cancel_handler = $std_core._makeHandler1( effCancel, 
    function( _loc, _x, _k ) { return _k(_x); },
    function(_l1, _l2, _cont, _op, _resume , _k ) { return _resume(_l1, _l2, _cont, $std_core._unit_, _k);  }
);


function _remove( arr, elem ) {
  var idx = arr.indexOf(elem);
  if (idx>=0) arr.splice(idx,1); // modify in-place
}

function _parent_cframe( hctx ) {
  if (hctx==null) hctx = $std_core.capture_handler_context();
  var top  = hctx.htop;
  while (top >= 0 && hctx.hstack[top].optag !== effCancel) { top--; }
  return (top >= 0 ? hctx.hstack[top].loc1 : null);
}

function _create_cancelable( caction ) {
  // identity operations
  var cframe = { canceled:false, contexts:[], nested:[], parent:_parent_cframe() };
  function cancel()   { return _cancel(cframe); }
  function action(_k) { return caction( cancel, _k); }  
  return $std_core._tuple2_( 
    (function(){ 
      if (cframe.parent) _remove(cframe.parent.nested,cframe); 
    }),
    (function(_k){ 
      if (cframe.parent) cframe.parent.nested.push(cframe);
      return _cancel_handler( cframe, action, _k ); 
    })
  );
}

function throw_canceled() {
  $std_core.throw_1("Canceled",Cancel);
}

function _cancel( cframe ) {
  if (cframe.canceled) return;
  cframe.canceled = true;
  var contexts = cframe.contexts; 
  var nested   = cframe.nested;
  cframe.contexts = [];
  cframe.nested   = [];
  contexts.forEach( function(hctx) { 
    if (hctx != null) {
      setTimeout( function() {
        $std_core.invoke_in_handler_context(hctx, function(_) {
          throw_canceled(); // throw a Cancel exception in that context
        });
      }, 0);
    }
  });
  nested.forEach( function(nestedFrame) {
    _cancel(nestedFrame);
  });
}

function _check_canceled( hctx ) {
  if (hctx==null) hctx = $std_core.capture_handler_context();
  var top = hctx.htop;
  while (top >= 0) {
    var h = hctx.hstack[top];
    if (h.optag===effCancel && h.loc1.canceled) {
      throw_canceled();
    }
    top--;
  }
}

function _register_cancel( cback ) {
  cback.cframe = _parent_cframe(cback.hctx);
  if (cback.cframe) {
    cback.cframe.contexts.push(cback.hctx);
  }
}

function _unregister_cancel( cback ) {
  if (cback.cframe) {
    _remove( cback.cframe.contexts, cback.hctx );
    cback.cframe = null;
  }
}



// --------------------------------------------------------
// Support for asynchronous actions with effect handlers
// --------------------------------------------------------

var _sync_nesting = 0;
var _sync_blocked = null;

function _async_start_synchronous() {
  _sync_nesting++;
  if (_sync_blocked===null) _sync_blocked = [];
}

function _async_end_synchronous() {
  if (_sync_nesting===0) return;
  _sync_nesting--; 
  if (_sync_nesting===0) {
    // now invoke all queued callbacks; we schedule all through a timeout
    _sync_blocked.forEach( function(entry) {
      var cb = function() { return entry.cback.invoke(entry.args); };
      setTimeout(cb,0);
    });
    _sync_blocked = null;
  }
}

function _Callback( cb, haserr, nodup ) {
  var self = this;
  self.cb     = cb;
  self.haserr = haserr;
  self.nodup  = nodup;
  self.cframe = null;
  // save the handler context
  self.hctx = $std_core.capture_handler_context();
  // remember if the callback itself was constructed inside a sync block
  self.in_synchronous = (_sync_nesting > 0); 
  // check for cancelation
  _check_canceled(self.hctx);
  _register_cancel(self);
}

_Callback.prototype.invoke = function(args) {
  var self = this;
  // unregister cancelation
  _unregister_cancel(self);
  // check if we return while a 'synchronous' block was started
  if (!self.in_synchronous && _sync_nesting > 0) { // if so, put ourselves on the queue
    if (self.nodup) {
      // update in-place possible previous occurrence of this event if 'nodup' was set (for timers or mouse events etc.)
      for (var i = 0; i < _sync_blocked.length; i++) {
        if (_sync_blocked[i].cback == self) {
          _sync_blocked[i].args = args;
          return; // updated in place, we are done.
        }
      }        
    }
    // and push ourselves on the queue to be called later
    _sync_blocked.push( { cback: self, args: args } );
    return;
  }
  else {
    // restore the handler context and invoke the callback
    $std_core.invoke_in_handler_context( self.hctx, function(_) {
      if (self.haserr) {
        if (args[0] != null) throw args[0];
        args.shift();         
      }
      return self.cb.apply(self.cb,args);
    }, $std_core._unit_ );
  }
}

// Create a callback wrapper around 'cb'. 
// If 'haserr' is true, the first parameter of the new call back is considered an error object
// and thrown automatically if not null. If it is null, it is not passed to the callback `cb`.
// If `nodup` is true, multiple call backs that are queued due to synchronized blocks are not duplicated
// and only the last call back is queued (for timers, mouse events etc).
function _async_callback( cb, haserr, nodup ) {
  var cback = new _Callback(cb,haserr,nodup);
  return (function(){ return cback.invoke(Array.prototype.slice.call(arguments)); });
}

function _async_fork( action ) {
  setTimeout( _async_callback( function() {
    action($std_core.async_yield);
  }), 0 );  
}

function _async_all( actions, k ) {
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
      // if an exception was set, raise it now (or keep raising on multiple resume!)
      if (exn!==undefined) throw exn;
      // in case a branch exits more than once (due to amb for example)
      // note: this should never happen as multiple resumes can only happen after
      // at least one resumption but that cannot be past an asyncAll..
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
      var cb = _async_callback( function(_) {
        return $std_core.$catch( function(k1) {
            return actions[j](k1);
          },
          function(_exn,_k2) {
            if (exn===undefined) exn = _exn;
            return allk(j, undefined, $std_core.async_yield);
          },
          function(x) { 
            return allk(j,x,$std_core.Result); 
          });
        }); 
      // by using setTimeout we can schedule all continuations for execution
      setTimeout(cb,0); 
    })(i);  // capture i by value
  }
  return $std_core.async_yield();
}
