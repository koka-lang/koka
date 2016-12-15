var $atoms       = {};
var $atoms_rev   = {};
var $unique      = 0;

function $atomCreate(s) {
  var i = $atoms[s];
  if (i===undefined) {
    i = $unique++;
    $atoms[s] = i;
    $atoms_rev[i] = s;
  }
  return i;
}

function $atomToString(i) {
  return $atoms_rev[i];
}

function $atomCreateMatcher(xs,ignoreCase) {
  var set = {};
  var i;
  for( i = 0; i < xs.length; i++) {
    var s = xs[i];
    if (ignoreCase !== 0) s = s.toLowerCase();
    set[s] = true;
  }
  return set;
}

function $atomMatch(set,s,ignoreCase)
{
  if (ignoreCase !== 0) s = s.toLowerCase();
  return (set[s] === true ? 1 : 0);
}
