/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var effCancel = "std/async/cancelable";

function _create_cancelable( caction ) {
  // identity operations
  function ret( loc, x, _k ) { return _k(x); };
  function ops(_l1, _l2, _cont, _op, _resume , _k ) { return _resume(_l1, _l2, _cont, _unit_, _k);  };
  var handle = $std_core._makeHandler1( effCancel, ret, ops );
  var loc = { canceled:false, cancelers:[] };
  function action(_k) { return caction( function(){ return _cancel(loc); }, _k); }
  return function(_k){ return handle( loc, action, _k ); } 
}

function throw_canceled() {
  $std_core.throw_1("Canceled",Cancel);
}

function _cancel( loc ) {
  loc.canceled = true;
  var cs = loc.cancelers; loc.cancelers = [];
  cs.forEach( function(cb) { 
    if (typeof cb.cancel === "function") { 
      cb.cancel();
      setTimeout( function() {
        $std_core._restore_handler_context(cb.hctx, function(_) {
          throw_canceled(); // throw a Cancel exception in that context
        });
      }, 0);
    }
  });
  //return $std_core._unit_;
}

function _check_canceled( hctx ) {
  if (hctx==null) hctx = $std_core._save_handler_context();
  var top = hctx.htop;
  while (top >= 0) {
    var h = hctx.hstack[top];
    if (h.optag===effCancel && h.loc1.canceled) throw_canceled();
    top--;
  }
  //return $std_core._unit_;
}

function _register_cancel( cb, f ) {
  var hctx = $std_core._save_handler_context();
  var top = hctx.htop;
  while (top >= 0 && hctx.hstack[top].optag !== effCancel) { top--; }
  if (top >= 0) {
    //console.log("set cancel: " + cb + ", f: " + f);
    // only register if a cancelation frame exists
    cb.loc = hctx.hstack[top].loc1
    cb.loc.cancelers.push(cb);
    cb.cancel = f;
  }
  //return $std_core._unit_;
}

function _unregister_cancel( cb ) {
  if (typeof cb.cancel === "function") {
    cb.loc.cancelers = cb.loc.cancelers.filter( function(f) { return (f !== cb); } );
  }
  //return $std_core._unit_;
}



// --------------------------------------------------------
// Support for asynchronous actions with effect handlers
// --------------------------------------------------------

var _syncNesting = 0;
var _syncBlocked = null;

function $asyncStartSynchronous() {
  _syncNesting++;
  if (_syncBlocked===null) _syncBlocked = [];
}

function $asyncEndSynchronous() {
  if (_syncNesting===0) return;
  _syncNesting--; 
  if (_syncNesting===0) {
    // now invoke all queued callbacks; we schedule all through a timeout
    _syncBlocked.forEach( function(entry) {
      var cb = function() { return entry.fun.apply(entry.fun,entry.args); };
      setTimeout(cb,0);
    });
    _syncBlocked = null;
  }
}

// Create a callback wrapper around 'cb'. 
// If 'haserr' is true, the first parameter of the new call back is considered an error object
// and thrown automatically if not null. If it is null, it is not passed to the callback `cb`.
// If `nodup` is true, multiple call backs that are queued due to synchronized blocks are not duplicated
// and only the last call back is queued (for timers, mouse events etc).
function $asyncCallback( cb, haserr, nodup ) {
  // save the handler context
  var _hctx = $std_core._save_handler_context();
  // check for cancelation
  _check_canceled(_hctx);
  // remember if this callback itself was constructed inside a sync block
  var _in_synchronous = (_syncNesting > 0); 
  // and return a wrapped callback that restores the context
  var self = null;
  self = (function() {
    var args = Array.prototype.slice.call(arguments);
    // unregister cancelation
    _unregister_cancel(self);
    // check if we return while a 'synchronous' block was started
    if (!self.in_synchronous && _syncNesting > 0) { // if so, put ourselves on the queue
      if (nodup) {
        // update in-place possible previous occurrence of this event if 'nodup' was set (for timers or mouse events etc.)
        for (var i = 0; i < _syncBlocked.length; i++) {
          if (_syncBlocked[i].fun == self) {
            _syncBlocked[i].args = args;
            return; // updated in place, we are done.
          }
        }        
      }
      // and push ourselves on the queue to be called later
      _syncBlocked.push( { fun: self, args: args } );
      return;
    }
    else {
      // restore the handler context and invoke the callback
      $std_core._restore_handler_context( self.hctx, function(_) {
        if (haserr) {
          if (args[0] != null) throw args[0];
          args.shift();         
        }
        return cb.apply(cb,args);
      }, $std_core._unit_ );
    }
  });
  self.hctx = _hctx;
  self.in_synchronous = _in_synchronous;
  self.loc = null;
  self.cancel = null;
  self.canceled = false;
  return self;
}

function $asyncFork( action ) {
  setTimeout( _asyncCallback( function() {
    action($std_core.async_yield);
  }), 0 );  
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
      var cb = $asyncCallback( function(_) {
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
