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

function _direct_op(eff) {
  var top = $std_core._htop();
  var bottom  = top;
  var hstack = $std_core._hstack();
  while (top >= 0 && hstack[top].optag !== eff._tag) {
    hstack[top].readonly = true;        
    top--;
  } 
  if (top < 0) {
  	throw "oops; op does not match";
  }
  var h = hstack[top];
  //$htop = top-1;

  var cont = $std_core.id;
  var res;
  if (h.localCount===0) {
    res = h.ops(hstack, top, cont, eff._field1, _direct_resume1, h.k);
  }
  else if (h.localCount===1) {
    res = h.ops(hstack, top, cont, eff._field1, _direct_resume1, h.loc1, h.k);
  }
  else {
    throw "too many locals";
  }
  return res;
}

function _direct_count(k) {
  while(1) {
    comp();
    var i = _direct_op(_Eff_state(_Op_get));
    if (i===0) {
      return k(i);
    }
    else {
      _direct_op(_Eff_state(_Op_put((i-1)|0)));
      continue;
    }
  }
}
