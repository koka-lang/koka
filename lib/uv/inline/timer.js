function _init_timer(){
  return {};
}
// Start a repeating timer that fires every `interval` ms.
function _start_timer(timer, interval, fcn){
  timer.id = setInterval(fcn, Number(interval));
  timer.repeating = true;
  return $std_core_types.Ok($std_core_types.Unit);
}

// Start a one-shot timer that fires `fcn` once after `timeout` ms.
// Wraps the callback so that the timer self-cleans when it fires.
function _start_timer_once(timer, timeout, fcn){
  timer.repeating = false;
  timer.id = setTimeout(function(){
    timer.id = null;
    fcn();
  }, Number(timeout));
  return $std_core_types.Ok($std_core_types.Unit);
}

// Stop the timer
function _stop_timer(timer){
  if (timer.id) {
    if (timer.repeating) {
      clearInterval(timer.id);
    } else {
      clearTimeout(timer.id);
    }
    timer.id = null;
  }
}


