function _init_timer(){
  return {};
}
// Start the timer, either as a one-shot or repeating timer
function _start_timer(timer, ms, repeat, fcn){
  const rp = Number(repeat)
  const msx = Number(ms)
  if (rp != 0) {
    timer.id = setInterval(fcn, rp);
    timer.repeat = rp;
  } else {
    timer.id = setTimeout(fcn, msx);
  } 
}

// Stop the timer
function _stop_timer(timer){
  if (timer.id) {
    if (timer.repeat != 0) {
      clearInterval(timer.id);
    } else {
      clearTimeout(timer.id);
    }
    timer.id = null;
  }
}