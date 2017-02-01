/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/



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
  var hctx = $std_core._save_handler_context();
  var self_in_synchronous = (_syncNesting > 0); // remember this callback itself was constructed inside a sync block
  // and return a wrapped callback that restores the context
  return (function() {
    var self = this;
    var args = Array.prototype.slice.call(arguments);
    // check if we return while a 'synchronous' block was started
    if (!self_in_synchronous && _syncNesting > 0) { // if so, put ourselves on the queue
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
      $std_core._restore_handler_context( hctx, function(_) {
        if (haserr) {
          if (args[0] != null) throw args[0];
          args.shift();         
        }
        return cb.apply(cb,args);
      }, $std_core._unit_ );
    }
  });
}

function $asyncFork( action ) {
  setTimeout( _asyncCallback( function() {
    action(async_yield);
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
