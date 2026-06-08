module async/web/evloop

import std/num/int64

extern import
  js file "evloop.js"

pub extern event-loop-init() : any 
  js inline "null"

pub extern event-loop-run(evloop : any) : ()
  js inline "$std_core_types.Unit"

pub extern timer-setup( evloop : any, millisecs : int64, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  js "kk_timer_setup"

pub extern immediate-setup( evloop : any, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  js "kk_immediate_setup"  