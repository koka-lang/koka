// Koka generated module: basic/garsia-wachs, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_bool from './std_core_bool.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_sslice from './std_core_sslice.mjs';
import * as $std_core_list from './std_core_list.mjs';
import * as $std_core_maybe from './std_core_maybe.mjs';
import * as $std_core_maybe2 from './std_core_maybe2.mjs';
import * as $std_core_either from './std_core_either.mjs';
import * as $std_core_result from './std_core_result.mjs';
import * as $std_core_tuple from './std_core_tuple.mjs';
import * as $std_core_lazy from './std_core_lazy.mjs';
import * as $std_core_show from './std_core_show.mjs';
import * as $std_core_debug from './std_core_debug.mjs';
import * as $std_core_delayed from './std_core_delayed.mjs';
import * as $std_core_console from './std_core_console.mjs';
import * as $std_core from './std_core.mjs';
 
// externals
 
// type declarations
// type list1
export function Cons1(head, tail) /* forall<a> (head : a, tail : list<a>) -> list1<a> */  {
  return { head: head, tail: tail };
}
// type tree
export function Leaf(value) /* forall<a> (value : a) -> tree<a> */  {
  return { _tag: 1, value: value };
}
export function Node(left, right) /* forall<a> (left : tree<a>, right : tree<a>) -> tree<a> */  {
  return { _tag: 2, left: left, right: right };
}
 
// declarations
 
 
// Automatically generated. Tests for the `Leaf` constructor of the `:tree` type.
export function is_leaf(tree) /* forall<a> (tree : tree<a>) -> bool */  {
  return (tree._tag === 1);
}
 
 
// Automatically generated. Tests for the `Node` constructor of the `:tree` type.
export function is_node(tree) /* forall<a> (tree : tree<a>) -> bool */  {
  return (tree._tag === 2);
}
 
export function tree_fs_show(t) /* (t : tree<string>) -> string */  {
  if (t._tag === 1) {
    return $std_core_show.string_fs_show(t.value);
  }
  else {
    return $std_core_types._lp__plus__plus__rp_("Node(", $std_core_types._lp__plus__plus__rp_(tree_fs_show(t.left), $std_core_types._lp__plus__plus__rp_(",", $std_core_types._lp__plus__plus__rp_(tree_fs_show(t.right), ")"))));
  }
}
 
 
// Automatically generated. Retrieves the `head` constructor field of the `:list1` type.
export function list1_fs_head(list1) /* forall<a> (list1 : list1<a>) -> a */  {
  return list1.head;
}
 
 
// Automatically generated. Retrieves the `tail` constructor field of the `:list1` type.
export function list1_fs_tail(list1) /* forall<a> (list1 : list1<a>) -> list<a> */  {
  return list1.tail;
}
 
export function list1_fs__copy(_this, head, tail) /* forall<a> (list1<a>, head : ? a, tail : ? (list<a>)) -> list1<a> */  {
  if (head !== undefined) {
    var _x0 = head;
  }
  else {
    var _x0 = _this.head;
  }
  if (tail !== undefined) {
    var _x1 = tail;
  }
  else {
    var _x1 = _this.tail;
  }
  return Cons1(_x0, _x1);
}
 
 
// monadic lift
export function _mlift_map_10049(_y_x10046, _y_x10047) /* forall<a,e> (a, list<a>) -> e list1<a> */  {
  return Cons1(_y_x10046, _y_x10047);
}
 
 
// monadic lift
export function _mlift_map_10050(f, ys, _y_x10046) /* forall<a,b,e> (f : (a) -> e b, ys : list<a>, b) -> e list1<b> */  {
   
  var x_10051 = $std_core_list.map(ys, f);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10047 /* list<523> */ ) {
      return Cons1(_y_x10046, _y_x10047);
    });
  }
  else {
    return Cons1(_y_x10046, x_10051);
  }
}
 
export function map(xs, f) /* forall<a,b,e> (xs : list1<a>, f : (a) -> e b) -> e list1<b> */  {
   
  var x_10055 = f(xs.head);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10046 /* 523 */ ) {
      return _mlift_map_10050(f, xs.tail, _y_x10046);
    });
  }
  else {
     
    var x_0_10058 = $std_core_list.map(xs.tail, f);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10047 /* list<523> */ ) {
        return Cons1(x_10055, _y_x10047);
      });
    }
    else {
      return Cons1(x_10055, x_0_10058);
    }
  }
}
 
export function zip(xs, ys) /* forall<a,b> (xs : list1<a>, ys : list1<b>) -> list1<(a, b)> */  {
  var _x2 = xs.head;
  var _x3 = ys.head;
  var _x4 = xs.tail;
  var _x5 = ys.tail;
  return Cons1($std_core_types.Tuple2(_x2, _x3), $std_core_list.zip(_x4, _x5));
}
 
export function extract(before, after) /* forall<a> (before : list<(tree<a>, int)>, after : list1<(tree<a>, int)>) -> div tree<a> */  { tailcall: while(1)
{
  if (after.tail === null) {
    return after.head.fst;
  }
  else {
    if (after.tail.tail === null) {
      return insert($std_core_types.Nil, $std_core_types.Tuple2(Node(after.head.fst, after.tail.head.fst), $std_core_types._int_add((after.head.snd),(after.tail.head.snd))), before);
    }
    else {
      if ($std_core_types._int_le((after.head.snd),(after.tail.tail.head.snd))) {
        return insert(after.tail.tail, $std_core_types.Tuple2(Node(after.head.fst, after.tail.head.fst), $std_core_types._int_add((after.head.snd),(after.tail.head.snd))), before);
      }
      else {
        {
          // tail call
          var _x6 = $std_core_types.Cons(after.head, before);
          var _x7 = Cons1(after.tail.head, after.tail.tail);
          before = _x6;
          after = _x7;
          continue tailcall;
        }
      }
    }
  }
}}
 
export function insert(after_0, t, before_0) /* forall<a> (after : list<(tree<a>, int)>, t : (tree<a>, int), before : list<(tree<a>, int)>) -> div tree<a> */  { tailcall: while(1)
{
  if (before_0 === null) {
    return extract($std_core_types.Nil, Cons1(t, after_0));
  }
  else {
    var _x9 = before_0.head.snd;
    var _x10 = t.snd;
    var _x8 = $std_core_types._int_lt(_x9,_x10);
    if (_x8) {
      {
        // tail call
        var _x11 = $std_core_types.Cons(before_0.head, after_0);
        after_0 = _x11;
        before_0 = before_0.tail;
        continue tailcall;
      }
    }
    else {
      if (before_0.tail === null) {
        return extract($std_core_types.Nil, Cons1(before_0.head, $std_core_types.Cons(t, after_0)));
      }
      else {
        return extract(before_0.tail.tail, Cons1(before_0.tail.head, $std_core_types.Cons(before_0.head, $std_core_types.Cons(t, after_0))));
      }
    }
  }
}}
 
export function balance(xs) /* forall<a> (xs : list1<(tree<a>, int)>) -> div tree<a> */  {
  return extract($std_core_types.Nil, xs);
}
 
export function mark(depth, t) /* forall<a,h> (depth : int, t : tree<(a, ref<h,int>)>) -> (write<h>) () */  { tailcall: while(1)
{
  if (t._tag === 1) {
    return (((t.value.snd)).value = depth);
  }
  else {
     
    mark($std_core_types._int_add(depth,1), t.left);
    {
      // tail call
      var _x12 = $std_core_types._int_add(depth,1);
      depth = _x12;
      t = t.right;
      continue tailcall;
    }
  }
}}
 
export function build(depth, xs) /* forall<a,h> (depth : int, xs : list1<(a, ref<h,int>)>) -> <div,read<h>> (tree<a>, list<(a, ref<h,int>)>) */  {
  var _x14 = xs.head.snd;
  var _x13 = $std_core_types._int_eq((_x14.value),depth);
  if (_x13) {
    var _x15 = xs.head.fst;
    var _x16 = xs.tail;
    return $std_core_types.Tuple2(Leaf(_x15), _x16);
  }
  else {
     
    var l = build($std_core_types._int_add(depth,1), xs);
    if (l.snd === null) {
      var _x17 = l.fst;
      return $std_core_types.Tuple2(_x17, $std_core_types.Nil);
    }
    else {
       
      var r = build($std_core_types._int_add(depth,1), Cons1(l.snd.head, l.snd.tail));
      var _x18 = l.fst;
      var _x19 = r.fst;
      var _x20 = r.snd;
      return $std_core_types.Tuple2(Node(_x18, _x19), _x20);
    }
  }
}
 
export function garsia_wachs(xs) /* forall<a> (xs : list1<(a, int)>) -> div tree<a> */  {
   
  function f_10031(_pat_x111__27) /* ((2168, int)) -> <alloc<_1890>,div,read<_1890>,write<_1890>> (2168, ref<_1890,int>) */  {
    return $std_core_types.Tuple2(_pat_x111__27.fst, { value: 0 });
  }
   
  var refs = Cons1(f_10031(xs.head), $std_core_list.map(xs.tail, f_10031));
   
  var xs_1_10032 = Cons1(Leaf(refs.head), $std_core_list.map(refs.tail, Leaf));
   
  var _x21 = xs.head.snd;
  var ys_0_10033 = Cons1(_x21, $std_core_list.map(xs.tail, $std_core_types.tuple2_fs_snd));
   
  var _x22 = xs_1_10032.head;
  var _x23 = ys_0_10033.head;
  var _x24 = xs_1_10032.tail;
  var _x25 = ys_0_10033.tail;
  var wleafs = Cons1($std_core_types.Tuple2(_x22, _x23), $std_core_list.zip(_x24, _x25));
   
  var tree = extract($std_core_types.Nil, wleafs);
   
  mark(0, tree);
   
  var tuple2_10043 = build(0, refs);
  return tuple2_10043.fst;
}
 
export function example() /* () -> div string */  {
   
  var tree = garsia_wachs(Cons1($std_core_types.Tuple2("a", 3), $std_core_types.Cons($std_core_types.Tuple2("b", 2), $std_core_types.Cons($std_core_types.Tuple2("c", 1), $std_core_types.Cons($std_core_types.Tuple2("d", 4), $std_core_types.Cons($std_core_types.Tuple2("e", 5), $std_core_types.Nil))))));
  return tree_fs_show(tree);
}
 
export function main() /* () -> <console/console,div> () */  {
   
  var s_10044 = example();
  return $std_core_console.printsln(s_10044);
}