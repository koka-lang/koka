module async/web/evloop

import std/num/int64

extern import
  c  file "evloop.c"
  js file "evloop.js"

pub extern event-loop-init() : any 
  c "kk_event_loop_init"
  js inline "null" 

pub extern event-loop-run(evloop : any) : ()
  c  "kk_event_loop_run"
  js inline "$std_core_types.Unit"

pub extern timer-setup( evloop : any, millisecs : int64, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_timer_setup"
  js "kk_timer_setup" 

pub extern immediate-setup( evloop : any, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_immediate_setup"
  js "kk_immediate_setup"  