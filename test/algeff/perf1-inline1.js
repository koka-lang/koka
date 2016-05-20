// Testing with a mode where an effect would
// be annotated to promise to only have a one-shot 
// handler. In that case no cps transformation is necessary.
//
// This improves the speed by about 2x !
//
var stateOpTag = "perf1/(.Eff-state)";

function _direct_resume1(hstack,top,cont,loc1,x,k) {
	var h = hstack[top];
  h.loc1 = loc1;
  h.k    = k;
  return x;
}

function _direct_op(op) {
  var top = $std_core._htop();
  var hstack = $std_core._hstack();
  var h = hstack[top];
  if (h.optag !== stateOpTag) {
  	throw "oops; op does not match";
  }
  var cont = $std_core.id;
  return h.ops(hstack, top, cont, op, _direct_resume1, h.loc1, h.k);
}

function _direct_count(k) {
  while(1) {
    var i = _direct_op(_Op_get);
    if (i===0) {
      return k(i);
    }
    else {
      _direct_op(_Op_put((i-1)|0));
      continue;
    }
  }
}
