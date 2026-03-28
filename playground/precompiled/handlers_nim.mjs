// Koka generated module: handlers/nim, koka version: 3.2.4
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
import * as $std_num_float64 from './std_num_float64.mjs';
import * as $std_num_random from './std_num_random.mjs';
 
// externals
 
// type declarations
 
 
// runtime tag for the effect `:move`
export var move_fs__tag;
var move_fs__tag = "move@nim";
 
 
// runtime tag for the effect `:cheat`
export var cheat_fs__tag;
var cheat_fs__tag = "cheat@nim";
 
 
// runtime tag for the effect `:choose`
export var choose_fs__tag;
var choose_fs__tag = "choose@nim";
 
 
// runtime tag for the effect `:state`
export var state_fs__tag;
var state_fs__tag = "state@nim";
// type player
export const Bob = 1; // player
export const Alice = 2; // player
// type cheat
export function _Hnd_cheat(_cfc, _fun_cheat) /* forall<e,a> (int, forall<b> hnd/clause1<player,b,cheat,e,a>) -> cheat<e,a> */  {
  return { _cfc: _cfc, _fun_cheat: _fun_cheat };
}
// type choose
export function _Hnd_choose(_cfc, _ctl_choose) /* forall<e,a> (int, hnd/clause0<bool,choose,e,a>) -> choose<e,a> */  {
  return { _cfc: _cfc, _ctl_choose: _ctl_choose };
}
// type gtree
export function Take(player, moves) /* (player : player, moves : list<(int, gtree)>) -> gtree */  {
  return { _tag: 1, player: player, moves: moves };
}
export function Winner(player) /* (player : player) -> gtree */  {
  return { _tag: 2, player: player };
}
// type move
export function _Hnd_move(_cfc, _ctl_move) /* forall<e,a> (int, hnd/clause2<player,int,int,move,e,a>) -> move<e,a> */  {
  return { _cfc: _cfc, _ctl_move: _ctl_move };
}
// type state
export function _Hnd_state(_cfc, _fun_get, _fun_put) /* forall<a,e,b> (int, hnd/clause0<a,state<a>,e,b>, hnd/clause1<a,(),state<a>,e,b>) -> state<a,e,b> */  {
  return { _cfc: _cfc, _fun_get: _fun_get, _fun_put: _fun_put };
}
 
// declarations
 
 
// Automatically generated. Tests for the `Bob` constructor of the `:player` type.
export function is_bob(player) /* (player : player) -> bool */  {
  return (player === 1);
}
 
 
// Automatically generated. Tests for the `Alice` constructor of the `:player` type.
export function is_alice(player) /* (player : player) -> bool */  {
  return (player === 2);
}
 
export function _lp__eq__eq__rp_(p1, p2) /* (p1 : player, p2 : player) -> bool */  {
  if (p1 === 1 && p2 === 1) {
    return true;
  }
  else if (p1 === 2 && p2 === 2) {
    return true;
  }
  else {
    return false;
  }
}
 
export function player_fs_show(p) /* (p : player) -> string */  {
  return (p === 1) ? "bob" : "alice";
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:move` type.
export function move_fs__cfc(move_0) /* forall<e,a> (move : move<e,a>) -> int */  {
  return move_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-move` constructor field of the `:move` type.
export function move_fs__ctl_move(move_0) /* forall<e,a> (move : move<e,a>) -> hnd/clause2<player,int,int,move,e,a> */  {
  return move_0._ctl_move;
}
 
 
// handler for the effect `:move`
export function move_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : move<e,b>, ret : (res : a) -> e b, action : () -> <move|e> a) -> e b */  {
  return $std_core_hnd._hhandle(move_fs__tag, hnd, ret, action);
}
 
 
// select `move` operation out of effect `:move`
export function move_fs__select(hnd) /* forall<e,a> (hnd : move<e,a>) -> hnd/clause2<player,int,int,move,e,a> */  {
  return hnd._ctl_move;
}
 
 
// Call the `ctl move` operation of the effect `:move`
export function move(player, sticks) /* (player : player, sticks : int) -> move int */  {
   
  var evx_10245 = $std_core_hnd._evv_at(0);
  return evx_10245.hnd._ctl_move(evx_10245.marker, evx_10245, player, sticks);
}
 
 
// monadic lift
export function _mlift_alice_turn_10220(n, _y_x10029) /* (n : int, int) -> move player */  {
  return bob_turn($std_core_types._int_sub(n,_y_x10029));
}
 
 
// monadic lift
export function _mlift_bob_turn_10221(n_0, _y_x10032) /* (n@0 : int, int) -> move player */  {
  return alice_turn($std_core_types._int_sub(n_0,_y_x10032));
}
 
export function alice_turn(n_1) /* (n : int) -> <div,move> player */  {
  if ($std_core_types._int_le(n_1,0)) {
    return Bob;
  }
  else {
     
    var evx_10252 = $std_core_hnd._evv_at(0);
     
    var x_10249 = evx_10252.hnd._ctl_move(evx_10252.marker, evx_10252, Alice, n_1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10029_0 /* int */ ) {
        return _mlift_alice_turn_10220(n_1, _y_x10029_0);
      });
    }
    else {
      return bob_turn($std_core_types._int_sub(n_1,x_10249));
    }
  }
}
 
export function bob_turn(n_0_0) /* (n : int) -> <div,move> player */  {
  if ($std_core_types._int_le(n_0_0,0)) {
    return Alice;
  }
  else {
     
    var evx_0_10259 = $std_core_hnd._evv_at(0);
     
    var x_1_10256 = evx_0_10259.hnd._ctl_move(evx_0_10259.marker, evx_0_10259, Bob, n_0_0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10032_0 /* int */ ) {
        return _mlift_bob_turn_10221(n_0_0, _y_x10032_0);
      });
    }
    else {
      return alice_turn($std_core_types._int_sub(n_0_0,x_1_10256));
    }
  }
}
 
export function game(n) /* (n : int) -> <div,move> player */  {
  return alice_turn(n);
}
 
export function perfect(_action) /* forall<a,e> (() -> <move|e> a) -> e a */  {
  return move_fs__handle(_Hnd_move(1, function(m /* hnd/marker<694,693> */ , ev /* hnd/ev<move> */ , x1 /* player */ , x2 /* int */ ) {
        return $std_core_hnd.under2(ev, function(__p /* player */ , n /* int */ ) {
             
            var j_10147 = $std_core_types._int_mod(n,4);
            return ($std_core_types._int_ge(1,j_10147)) ? 1 : j_10147;
          }, x1, x2);
      }), function(_res /* 693 */ ) {
      return _res;
    }, _action);
}
 
export function example_perfect1() /* () -> div player */  {
  return perfect(function() {
    return alice_turn(7);
  });
}
 
export function example_perfect2() /* () -> div player */  {
  return perfect(function() {
    return alice_turn(12);
  });
}
 
 
// Automatically generated. Tests for the `Take` constructor of the `:gtree` type.
export function is_take(gtree) /* (gtree : gtree) -> bool */  {
  return (gtree._tag === 1);
}
 
 
// Automatically generated. Retrieves the `player` constructor field of the `:gtree` type.
export function gtree_fs_player(gtree) /* (gtree : gtree) -> player */  {
  return (gtree._tag === 1) ? gtree.player : gtree.player;
}
 
 
// Automatically generated. Tests for the `Winner` constructor of the `:gtree` type.
export function is_winner(gtree) /* (gtree : gtree) -> bool */  {
  return (gtree._tag === 2);
}
 
export function valid_moves(n) /* (n : int) -> list<int> */  {
  return $std_core_list.filter($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))), function(m /* int */ ) {
      return $std_core_types._int_le(m,n);
    });
}
 
 
// monadic lift
export function _mlift_gametree_10222(moves, p, subgames) /* forall<e> (moves : list<int>, p : player, subgames : list<gtree>) -> e gtree */  {
   
  var subtrees = $std_core_list.zip(moves, subgames);
  return Take(p, subtrees);
}
 
export function gametree(_action) /* forall<e> (() -> <move|e> player) -> e gtree */  {
  return move_fs__handle(_Hnd_move(3, function(m /* hnd/marker<1028,gtree> */ , ___wildcard_x726__16 /* hnd/ev<move> */ , x1 /* player */ , x2 /* int */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<int,gtree>) -> 1028 gtree */ ) {
            return $std_core_hnd.protect2(x1, x2, function(p /* player */ , n /* int */ , resume /* (int) -> 1028 gtree */ ) {
                 
                var moves = valid_moves(n);
                 
                var x_10265 = $std_core_list.map(moves, resume);
                 
                function next_10266(subgames) /* (list<gtree>) -> 1028 gtree */  {
                   
                  var subtrees = $std_core_list.zip(moves, subgames);
                  return Take(p, subtrees);
                }
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(next_10266);
                }
                else {
                  return next_10266(x_10265);
                }
              }, k);
          });
      }), function(x_0 /* player */ ) {
      return Winner(x_0);
    }, _action);
}
 
export function show_gtree(gt, indent) /* (gt : gtree, indent : int) -> div string */  {
  if (gt._tag === 2) {
    var _x0 = (gt.player === 1) ? "bob" : "alice";
    return $std_core_types._lp__plus__plus__rp_(_x0, " wins");
  }
  else {
     
    var lines = $std_core_list.map(gt.moves, function(_pat_x101__32 /* (int, gtree) */ ) {
        return $std_core_types._lp__plus__plus__rp_("\n", $std_core_types._lp__plus__plus__rp_($std_core_string.repeatz(" ", $std_core_int.ssize__t(indent)), $std_core_types._lp__plus__plus__rp_($std_core_int.show(_pat_x101__32.fst), $std_core_types._lp__plus__plus__rp_(" -> ", show_gtree(_pat_x101__32.snd, $std_core_types._int_add(indent,2))))));
      });
    var _x1 = (gt.player === 1) ? "bob" : "alice";
    return $std_core_types._lp__plus__plus__rp_(_x1, $std_core_list.concat_fs_join(lines));
  }
}
 
export function gtree_fs_show(gt) /* (gt : gtree) -> div string */  {
  return show_gtree(gt, 2);
}
 
export function example_gtree() /* () -> div gtree */  {
  return gametree(function() {
    return alice_turn(3);
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:cheat` type.
export function cheat_fs__cfc(cheat_0) /* forall<e,a> (cheat : cheat<e,a>) -> int */  {
  return cheat_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@fun-cheat` constructor field of the `:cheat` type.
export function cheat_fs__fun_cheat(cheat_0) /* forall<e,a,b> (cheat : cheat<e,a>) -> hnd/clause1<player,b,cheat,e,a> */  {
  return cheat_0._fun_cheat;
}
 
 
// handler for the effect `:cheat`
export function cheat_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : cheat<e,b>, ret : (res : a) -> e b, action : () -> <cheat|e> a) -> e b */  {
  return $std_core_hnd._hhandle(cheat_fs__tag, hnd, ret, action);
}
 
 
// select `cheat` operation out of effect `:cheat`
export function cheat_fs__select(hnd) /* forall<a,e,b> (hnd : cheat<e,b>) -> hnd/clause1<player,a,cheat,e,b> */  {
  return hnd._fun_cheat;
}
 
 
// Call the `fun cheat` operation of the effect `:cheat`
export function cheat(player) /* forall<a> (player : player) -> cheat a */  {
   
  var ev_10271 = $std_core_hnd._evv_at(0);
  var _x2 = ev_10271.hnd._fun_cheat;
  return _x2(ev_10271.marker, ev_10271, player);
}
 
export function cheat_report(_action) /* forall<a,e> (() -> <cheat,exn|e> a) -> <exn|e> a */  {
  return cheat_fs__handle(_Hnd_cheat(1, $std_core_hnd.clause_tail1(function(p /* player */ ) {
         
        var _x_x1_10190 = $std_core_types._lp__plus__plus__rp_($std_core_hnd._open_none1(function(p_0 /* player */ ) {
              return (p_0 === 1) ? "bob" : "alice";
            }, p), " cheated!");
        return $std_core_hnd._open_at2($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), $std_core_exn.$throw, _x_x1_10190);
      })), function(_res /* 1826 */ ) {
      return _res;
    }, _action);
}
 
 
// monadic lift
export function _mlift_check_10223(m, p, resume, _y_x10048) /* forall<a,e> (m : int, p : player, resume : (int) -> <cheat,move|e> a, maybe<int>) -> <cheat,move|e> a */  {
  var _x3 = $std_core_hnd._open_none1(function(mb /* maybe<int> */ ) {
      return (mb !== null);
    }, _y_x10048);
  if (_x3) {
    return resume(m);
  }
  else {
    return $std_core_hnd._open_at1($std_core_hnd._evv_index(cheat_fs__tag), function(player_0 /* player */ ) {
         
        var ev_10274 = $std_core_hnd._evv_at(0);
        var _x4 = ev_10274.hnd._fun_cheat;
        return _x4(ev_10274.marker, ev_10274, player_0);
      }, p);
  }
}
 
 
// monadic lift
export function _mlift_check_10224(n, p, resume, m) /* forall<a,e> (n : int, p : player, resume : (int) -> <cheat,move|e> a, m : int) -> <move,cheat|e> a */  {
   
  var x_10277 = $std_core_list.find($std_core_hnd._open_none1(valid_moves, n), function(i /* int */ ) {
      return $std_core_types._int_eq(i,m);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10048 /* maybe<int> */ ) {
      return _mlift_check_10223(m, p, resume, _y_x10048);
    });
  }
  else {
    return _mlift_check_10223(m, p, resume, x_10277);
  }
}
 
export function check(_action) /* forall<a,e> (() -> <cheat,move,move|e> a) -> <cheat,move|e> a */  {
  return move_fs__handle(_Hnd_move(3, function(m /* hnd/marker<<cheat,move|2081>,2080> */ , ___wildcard_x726__16 /* hnd/ev<move> */ , x1 /* player */ , x2 /* int */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<int,2080>) -> <cheat,move|2081> 2080 */ ) {
            return $std_core_hnd.protect2(x1, x2, function(p /* player */ , n /* int */ , resume /* (int) -> <cheat,move|2081> 2080 */ ) {
                 
                var x_10280 = $std_core_hnd._open_at2($std_core_hnd._evv_index(move_fs__tag), function(player /* player */ , sticks /* int */ ) {
                     
                    var evx_10282 = $std_core_hnd._evv_at(0);
                    return evx_10282.hnd._ctl_move(evx_10282.marker, evx_10282, player, sticks);
                  }, p, n);
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(m_1 /* int */ ) {
                    return _mlift_check_10224(n, p, resume, m_1);
                  });
                }
                else {
                  return _mlift_check_10224(n, p, resume, x_10280);
                }
              }, k);
          });
      }), function(_res /* 2080 */ ) {
      return _res;
    }, _action);
}
 
export function example_check() /* () -> pure player */  {
  return perfect(function() {
    return cheat_report(function() {
      return check(function() {
        return $std_core_hnd._open_at1(2, alice_turn, 7);
      });
    });
  });
}
 
export function example_use() /* () -> list<int> */  {
  return $std_core_list.map($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))), function(x /* int */ ) {
      return $std_core_types._int_add(x,2);
    });
}
 
export function pc(_action) /* forall<a,e> (() -> <move|e> a) -> e a */  {
  return move_fs__handle(_Hnd_move(1, function(m /* hnd/marker<2323,2322> */ , ev /* hnd/ev<move> */ , x1 /* player */ , x2 /* int */ ) {
        return $std_core_hnd.under2(ev, function(p /* player */ , n /* int */ ) {
            if (p === 2) {
               
              var j_10156 = $std_core_types._int_mod(n,4);
              return ($std_core_types._int_ge(1,j_10156)) ? 1 : j_10156;
            }
            else {
              return n;
            }
          }, x1, x2);
      }), function(_res /* 2322 */ ) {
      return _res;
    }, _action);
}
 
export function example_pc1() /* () -> pure player */  {
  return pc(function() {
    return cheat_report(function() {
      return check(function() {
        return $std_core_hnd._open_at1(2, alice_turn, 12);
      });
    });
  });
}
 
export function example_pc2() /* () -> pure player */  {
  return perfect(function() {
    return cheat_report(function() {
      return check(function() {
        return pc(function() {
          return $std_core_hnd._open_at1(2, alice_turn, 12);
        });
      });
    });
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:choose` type.
export function choose_fs__cfc(choose_0) /* forall<e,a> (choose : choose<e,a>) -> int */  {
  return choose_0._cfc;
}
 
 
// Automatically generated. Retrieves the `@ctl-choose` constructor field of the `:choose` type.
export function choose_fs__ctl_choose(choose_0) /* forall<e,a> (choose : choose<e,a>) -> hnd/clause0<bool,choose,e,a> */  {
  return choose_0._ctl_choose;
}
 
 
// handler for the effect `:choose`
export function choose_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : choose<e,b>, ret : (res : a) -> e b, action : () -> <choose|e> a) -> e b */  {
  return $std_core_hnd._hhandle(choose_fs__tag, hnd, ret, action);
}
 
 
// select `choose` operation out of effect `:choose`
export function choose_fs__select(hnd) /* forall<e,a> (hnd : choose<e,a>) -> hnd/clause0<bool,choose,e,a> */  {
  return hnd._ctl_choose;
}
 
 
// Call the `ctl choose` operation of the effect `:choose`
export function choose() /* () -> choose bool */  {
   
  var ev_10288 = $std_core_hnd._evv_at(0);
  return ev_10288.hnd._ctl_choose(ev_10288.marker, ev_10288);
}
 
 
// monadic lift
export function _mlift_bob_chooses_10225(m, _y_x10072) /* forall<a,e> (m : () -> <move,choose|e> a, bool) -> <choose|e> a */  {
  if (_y_x10072) {
    return pc(m);
  }
  else {
    return perfect(m);
  }
}
 
export function bob_chooses(m) /* forall<a,e> (m : () -> <choose,move|e> a) -> <choose|e> a */  {
   
  var x_10290 = $std_core_hnd._open_at0($std_core_hnd._evv_index(choose_fs__tag), function() {
       
      var ev_10293 = $std_core_hnd._evv_at(0);
      return ev_10293.hnd._ctl_choose(ev_10293.marker, ev_10293);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10072 /* bool */ ) {
      if (_y_x10072) {
        return pc(m);
      }
      else {
        return perfect(m);
      }
    });
  }
  else {
    if (x_10290) {
      return pc(m);
    }
    else {
      return perfect(m);
    }
  }
}
 
 
// monadic lift
export function _mlift_all_results_10226(_y_x10076, _y_x10077) /* forall<a,e> (list<a>, list<a>) -> e list<a> */  {
  return $std_core_list.append(_y_x10076, _y_x10077);
}
 
 
// monadic lift
export function _mlift_all_results_10227(resume, _y_x10076) /* forall<a,e> (resume : (bool) -> e list<a>, list<a>) -> e list<a> */  {
   
  var x_10297 = resume(false);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10077 /* list<2749> */ ) {
      return $std_core_list.append(_y_x10076, _y_x10077);
    });
  }
  else {
    return $std_core_list.append(_y_x10076, x_10297);
  }
}
 
export function all_results(_action) /* forall<a,e> (() -> <choose|e> a) -> e list<a> */  {
  return choose_fs__handle(_Hnd_choose(3, function(m /* hnd/marker<2750,list<2749>> */ , ___wildcard_x688__16 /* hnd/ev<choose> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<bool,list<2749>>) -> 2750 list<2749> */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> 2750 list<2749> */ ) {
                 
                var x_10302 = r(true);
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(_y_x10076 /* list<2749> */ ) {
                    return _mlift_all_results_10227(r, _y_x10076);
                  });
                }
                else {
                  return _mlift_all_results_10227(r, x_10302);
                }
              }, k);
          });
      }), function(x_0 /* 2749 */ ) {
      return $std_core_types.Cons(x_0, $std_core_types.Nil);
    }, _action);
}
 
 
// monadic lift
export function _mlift_example_choose_10228(_y_x10079) /* (player) -> <div,move,choose> list<player> */  {
  return $std_core_types.Cons(_y_x10079, $std_core_types.Nil);
}
 
export function example_choose() /* () -> div list<player> */  {
  return choose_fs__handle(_Hnd_choose(3, function(m /* hnd/marker<div,list<player>> */ , ___wildcard_x688__16 /* hnd/ev<choose> */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<bool,list<player>>) -> div list<player> */ ) {
            return $std_core_hnd.protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (bool) -> div list<player> */ ) {
                 
                var xs_10015 = r(true);
                 
                var ys_10016 = r(false);
                return $std_core_list.append(xs_10015, ys_10016);
              }, k);
          });
      }), function(_res /* list<player> */ ) {
      return _res;
    }, function() {
      return bob_chooses(function() {
         
        var x_10305 = $std_core_hnd._open_at1(1, alice_turn, 7);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_example_choose_10228);
        }
        else {
          return $std_core_types.Cons(x_10305, $std_core_types.Nil);
        }
      });
    });
}
 
export function coin(_action) /* forall<a,e> (() -> <choose,ndet|e> a) -> <ndet|e> a */  {
  return choose_fs__handle(_Hnd_choose(1, $std_core_hnd.clause_tail0(function() {
        return (($std_num_random.srandom_float64()) > (0.5));
      })), function(_res /* 2966 */ ) {
      return _res;
    }, _action);
}
 
export function example_coin() /* () -> <div,ndet> player */  {
  return coin(function() {
    return bob_chooses(function() {
      return $std_core_hnd._open_at1(1, alice_turn, 7);
    });
  });
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:state` type.
export function state_fs__cfc(state_0) /* forall<a,e,b> (state : state<a,e,b>) -> int */  {
  return state_0._cfc;
}
 
 
// handler for the effect `:state`
export function state_fs__handle(hnd, ret, action) /* forall<a,b,e,c> (hnd : state<a,e,c>, ret : (res : b) -> e c, action : () -> <state<a>|e> b) -> e c */  {
  return $std_core_hnd._hhandle(state_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-get` constructor field of the `:state` type.
export function state_fs__fun_get(state_0) /* forall<a,e,b> (state : state<a,e,b>) -> hnd/clause0<a,state<a>,e,b> */  {
  return state_0._fun_get;
}
 
 
// select `get` operation out of effect `:state`
export function get_fs__select(hnd) /* forall<a,e,b> (hnd : state<a,e,b>) -> hnd/clause0<a,state<a>,e,b> */  {
  return hnd._fun_get;
}
 
 
// Call the `fun get` operation of the effect `:state`
export function get() /* forall<a> () -> (state<a>) a */  {
   
  var ev_10308 = $std_core_hnd._evv_at(0);
  return ev_10308.hnd._fun_get(ev_10308.marker, ev_10308);
}
 
 
// Automatically generated. Retrieves the `@fun-put` constructor field of the `:state` type.
export function state_fs__fun_put(state_0) /* forall<a,e,b> (state : state<a,e,b>) -> hnd/clause1<a,(),state<a>,e,b> */  {
  return state_0._fun_put;
}
 
 
// select `put` operation out of effect `:state`
export function put_fs__select(hnd) /* forall<a,e,b> (hnd : state<a,e,b>) -> hnd/clause1<a,(),state<a>,e,b> */  {
  return hnd._fun_put;
}
 
 
// Call the `fun put` operation of the effect `:state`
export function put(x) /* forall<a> (x : a) -> (state<a>) () */  {
   
  var ev_10310 = $std_core_hnd._evv_at(0);
  return ev_10310.hnd._fun_put(ev_10310.marker, ev_10310, x);
}
 
export function state(init, action) /* forall<a,b,e> (init : a, action : () -> <state<a>|e> b) -> e b */  {
  return function() {
     
    var loc = { value: init };
     
    var res = state_fs__handle(_Hnd_state(1, $std_core_hnd.clause_tail0(function() {
          return ((loc).value);
        }), $std_core_hnd.clause_tail1(function(x /* 3575 */ ) {
          return ((loc).value = x);
        })), function(_res /* 3576 */ ) {
        return _res;
      }, action);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
export var s0;
var s0 = $std_core_types.Cons($std_core_types.Tuple2(Alice, 0), $std_core_types.Cons($std_core_types.Tuple2(Bob, 0), $std_core_types.Nil));
 
export function update_score(p, gs) /* (p : player, gs : gstate) -> gstate */  {
  return $std_core_list.map(gs, function(_pat_x228__14 /* (player, int) */ ) {
      if (p === 1 && _pat_x228__14.fst === 1) {
        return $std_core_types.Tuple2(_pat_x228__14.fst, $std_core_types._int_add((_pat_x228__14.snd),1));
      }
      else if (p === 2 && _pat_x228__14.fst === 2) {
        return $std_core_types.Tuple2(_pat_x228__14.fst, $std_core_types._int_add((_pat_x228__14.snd),1));
      }
      else {
        return $std_core_types.Tuple2(_pat_x228__14.fst, _pat_x228__14.snd);
      }
    });
}
 
 
// monadic lift
export function _mlift_score_updater_10229(p_5262, wild__) /* forall<e> (p@5262 : player, wild_ : ()) -> <state<gstate>|e> player */  {
  return p_5262;
}
 
 
// monadic lift
export function _mlift_score_updater_10230(p_5262, _y_x10096) /* forall<e> (p@5262 : player, gstate) -> <state<gstate>|e> player */  {
   
  var _x_x1_10203 = $std_core_hnd._open_none2(function(p /* player */ , gs /* gstate */ ) {
      return $std_core_list.map(gs, function(_pat_x228__14 /* (player, int) */ ) {
          if (p === 1 && _pat_x228__14.fst === 1) {
            return $std_core_types.Tuple2(_pat_x228__14.fst, $std_core_types._int_add((_pat_x228__14.snd),1));
          }
          else if (p === 2 && _pat_x228__14.fst === 2) {
            return $std_core_types.Tuple2(_pat_x228__14.fst, $std_core_types._int_add((_pat_x228__14.snd),1));
          }
          else {
            return $std_core_types.Tuple2(_pat_x228__14.fst, _pat_x228__14.snd);
          }
        });
    }, p_5262, _y_x10096);
   
  var x_10316 = $std_core_hnd._open_at1($std_core_hnd._evv_index(state_fs__tag), function(x_0 /* gstate */ ) {
       
      var ev_10318 = $std_core_hnd._evv_at(0);
      return ev_10318.hnd._fun_put(ev_10318.marker, ev_10318, x_0);
    }, _x_x1_10203);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return p_5262;
    });
  }
  else {
    return p_5262;
  }
}
 
 
// monadic lift
export function _mlift_score_updater_10231(p_5262) /* forall<e> (p@5262 : player) -> <state<gstate>|e> player */  {
   
  var x_10323 = $std_core_hnd._open_at0($std_core_hnd._evv_index(state_fs__tag), function() {
       
      var ev_10325 = $std_core_hnd._evv_at(0);
      return ev_10325.hnd._fun_get(ev_10325.marker, ev_10325);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10096 /* gstate */ ) {
      return _mlift_score_updater_10230(p_5262, _y_x10096);
    });
  }
  else {
    return _mlift_score_updater_10230(p_5262, x_10323);
  }
}
 
export function score_updater(_action) /* forall<e> (() -> <state<gstate>|e> player) -> <state<gstate>|e> player */  {
   
  var x_10327 = _action();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(p_5262 /* player */ ) {
      return _mlift_score_updater_10231(p_5262);
    });
  }
  else {
     
    var x_0_10330 = $std_core_hnd._open_at0($std_core_hnd._evv_index(state_fs__tag), function() {
         
        var ev_10333 = $std_core_hnd._evv_at(0);
        return ev_10333.hnd._fun_get(ev_10333.marker, ev_10333);
      });
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10096 /* gstate */ ) {
        return _mlift_score_updater_10230(x_10327, _y_x10096);
      });
    }
    else {
       
      var _x_x1_10203 = $std_core_hnd._open_none2(function(p /* player */ , gs /* gstate */ ) {
          return $std_core_list.map(gs, function(_pat_x228__14 /* (player, int) */ ) {
              if (p === 1 && _pat_x228__14.fst === 1) {
                return $std_core_types.Tuple2(_pat_x228__14.fst, $std_core_types._int_add((_pat_x228__14.snd),1));
              }
              else if (p === 2 && _pat_x228__14.fst === 2) {
                return $std_core_types.Tuple2(_pat_x228__14.fst, $std_core_types._int_add((_pat_x228__14.snd),1));
              }
              else {
                return $std_core_types.Tuple2(_pat_x228__14.fst, _pat_x228__14.snd);
              }
            });
        }, x_10327, x_0_10330);
       
      var x_1_10335 = $std_core_hnd._open_at1($std_core_hnd._evv_index(state_fs__tag), function(x_2 /* gstate */ ) {
           
          var ev_0_10338 = $std_core_hnd._evv_at(0);
          return ev_0_10338.hnd._fun_put(ev_0_10338.marker, ev_0_10338, x_2);
        }, _x_x1_10203);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
          return x_10327;
        });
      }
      else {
        return x_10327;
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_print_board_10232(scores) /* (scores : list<string>) -> <console/console,alloc<global>,div,exn,fsys,ndet,net,read<global>,ui,write<global>> () */  {
  return $std_core_console.printsln($std_core_list.joinsep(scores, "\n"));
}
 
export function print_board(gs) /* (gs : gstate) -> io () */  {
   
  var x_10343 = $std_core_list.map(gs, function(_pat_x234__26 /* (player, int) */ ) {
      return $std_core_types._lp__plus__plus__rp_($std_core_hnd._open_none1(function(p_0 /* player */ ) {
            return (p_0 === 1) ? "bob" : "alice";
          }, _pat_x234__26.fst), $std_core_types._lp__plus__plus__rp_(" -> ", $std_core_int.show(_pat_x234__26.snd)));
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_print_board_10232);
  }
  else {
    return $std_core_console.printsln($std_core_list.joinsep(x_10343, "\n"));
  }
}
 
 
// monadic lift
export function _mlift_printer_10233(x_5263, wild__) /* forall<a,e> (x@5263 : a, wild_ : ()) -> <alloc<global>,console/console,div,exn,fsys,ndet,net,read<global>,ui,write<global>,state<gstate>|e> a */  {
  return x_5263;
}
 
 
// monadic lift
export function _mlift_printer_10234(x_5263, _y_x10102) /* forall<a,e> (x@5263 : a, gstate) -> <state<gstate>,alloc<global>,console/console,div,exn,fsys,ndet,net,read<global>,ui,write<global>|e> a */  {
   
  var x_10346 = $std_core_hnd._open_at1($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), print_board, _y_x10102);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x_5263;
    });
  }
  else {
    return x_5263;
  }
}
 
 
// monadic lift
export function _mlift_printer_10235(x_5263) /* forall<a,e> (x@5263 : a) -> <alloc<global>,console/console,div,exn,fsys,ndet,net,read<global>,state<gstate>,ui,write<global>|e> a */  {
   
  var x_10350 = $std_core_hnd._open_at0($std_core_hnd._evv_index(state_fs__tag), function() {
       
      var ev_10352 = $std_core_hnd._evv_at(0);
      return ev_10352.hnd._fun_get(ev_10352.marker, ev_10352);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10102 /* gstate */ ) {
      return _mlift_printer_10234(x_5263, _y_x10102);
    });
  }
  else {
    return _mlift_printer_10234(x_5263, x_10350);
  }
}
 
export function printer(_action) /* forall<a,e> (() -> <io,state<gstate>|e> a) -> <io,state<gstate>|e> a */  {
   
  var x_10354 = _action();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(x_5263 /* 4184 */ ) {
      return _mlift_printer_10235(x_5263);
    });
  }
  else {
     
    var x_0_10357 = $std_core_hnd._open_at0($std_core_hnd._evv_index(state_fs__tag), function() {
         
        var ev_10360 = $std_core_hnd._evv_at(0);
        return ev_10360.hnd._fun_get(ev_10360.marker, ev_10360);
      });
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10102 /* gstate */ ) {
        return _mlift_printer_10234(x_10354, _y_x10102);
      });
    }
    else {
       
      var x_1_10362 = $std_core_hnd._open_at1($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), print_board, x_0_10357);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
          return x_10354;
        });
      }
      else {
        return x_10354;
      }
    }
  }
}
 
export function example_print1() /* () -> io player */  {
  return state(s0, function() {
      return printer(function() {
        return coin(function() {
          return bob_chooses(function() {
            return score_updater(function() {
              return $std_core_hnd._open_at1(2, alice_turn, 7);
            });
          });
        });
      });
    });
}
 
 
// monadic lift
export function _mlift_replay_10236(action, n, x_5265) /* forall<a,e> (action : () -> <div|e> a, n : int, x@5265 : a) -> <div|e> a */  {
  if ($std_core_types._int_le(n,1)) {
    return x_5265;
  }
  else {
    return replay($std_core_types._int_sub(n,1), action);
  }
}
 
export function replay(n_0, action_0) /* forall<a,e> (n : int, action : () -> <div|e> a) -> <div|e> a */  { tailcall: while(1)
{
   
  var x_10367 = action_0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(x_5265_0 /* 4354 */ ) {
      return _mlift_replay_10236(action_0, n_0, x_5265_0);
    });
  }
  else {
    if ($std_core_types._int_le(n_0,1)) {
      return x_10367;
    }
    else {
      {
        // tail call
        var _x5 = $std_core_types._int_sub(n_0,1);
        n_0 = _x5;
        continue tailcall;
      }
    }
  }
}}
 
export function example_print2() /* () -> io player */  {
  return state(s0, function() {
      return printer(function() {
        return replay(10, function() {
            return coin(function() {
              return bob_chooses(function() {
                return score_updater(function() {
                  return $std_core_hnd._open_at1(2, alice_turn, 7);
                });
              });
            });
          });
      });
    });
}
 
 
// monadic lift
export function _mlift_main_10237(_y_x10140) /* (player) -> <div,exn,console/console,exn> () */  {
  return $std_core_console.printsln($std_core_hnd._open_none1(function(p_3 /* player */ ) {
      return (p_3 === 1) ? "bob" : "alice";
    }, _y_x10140));
}
 
 
// monadic lift
export function _mlift_main_10238(_y_x10121, _y_x10123, _y_x10125, _y_x10129, _y_x10134, _y_x10135) /* (player, player, gtree, player, player, string) -> <div,exn,console/console> () */  {
   
  $std_core_console.prints($std_core_list.joinsep($std_core_types.Cons("", $std_core_types.Cons($std_core_hnd._open_none1(function(p /* player */ ) {
            return (p === 1) ? "bob" : "alice";
          }, _y_x10121), $std_core_types.Cons($std_core_hnd._open_none1(function(p_0 /* player */ ) {
              return (p_0 === 1) ? "bob" : "alice";
            }, _y_x10123), $std_core_types.Cons($std_core_hnd._open_none1(function(gt /* gtree */ ) {
                return show_gtree(gt, 2);
              }, _y_x10125), $std_core_types.Cons($std_core_hnd._open_none1(function(p_1 /* player */ ) {
                  return (p_1 === 1) ? "bob" : "alice";
                }, _y_x10129), $std_core_types.Cons($std_core_hnd._open_none1(function(p_2 /* player */ ) {
                    return (p_2 === 1) ? "bob" : "alice";
                  }, _y_x10134), $std_core_types.Cons(_y_x10135, $std_core_types.Cons("", $std_core_types.Nil)))))))), "\n-----------\n"));
  return $std_core_exn.exn_fs__handle($std_core_exn._Hnd_exn(0, function(m /* hnd/marker<<console/console,div,exn>,()> */ , ___wildcard_x654__16 /* hnd/ev<exn> */ , x /* exception */ ) {
        return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<10004,()>) -> <console/console,div,exn> () */ ) {
            return $std_core_console.printsln($std_core_hnd._open_none1(function(exn_0 /* exception */ ) {
                return exn_0.message;
              }, x));
          });
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_0_10371 = $std_core_hnd._open_at0(0, function() {
          return pc(function() {
            return cheat_report(function() {
              return check(function() {
                return $std_core_hnd._open_at1(2, alice_turn, 12);
              });
            });
          });
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_main_10237);
      }
      else {
        return $std_core_console.printsln($std_core_hnd._open_none1(function(p_3 /* player */ ) {
            return (p_3 === 1) ? "bob" : "alice";
          }, x_0_10371));
      }
    });
}
 
 
// monadic lift
export function _mlift_main_10239(_y_x10121, _y_x10123, _y_x10125, _y_x10129, _y_x10134) /* (player, player, gtree, player, player) -> <div,exn> () */  {
   
  var x_10373 = $std_core_list.show($std_core_hnd._open_none0(example_choose), function(_x_x1_1 /* player */ ) {
      return $std_core_hnd._open_none1(player_fs_show, _x_x1_1);
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10135 /* string */ ) {
      return _mlift_main_10238(_y_x10121, _y_x10123, _y_x10125, _y_x10129, _y_x10134, _y_x10135);
    });
  }
  else {
    return _mlift_main_10238(_y_x10121, _y_x10123, _y_x10125, _y_x10129, _y_x10134, x_10373);
  }
}
 
 
// monadic lift
export function _mlift_main_10240(_y_x10121, _y_x10123, _y_x10125, _y_x10129) /* (player, player, gtree, player) -> <div,exn> () */  {
   
  var x_10375 = perfect(function() {
    return cheat_report(function() {
      return check(function() {
        return pc(function() {
          return $std_core_hnd._open_at1(2, alice_turn, 12);
        });
      });
    });
  });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10134 /* player */ ) {
      return _mlift_main_10239(_y_x10121, _y_x10123, _y_x10125, _y_x10129, _y_x10134);
    });
  }
  else {
    return _mlift_main_10239(_y_x10121, _y_x10123, _y_x10125, _y_x10129, x_10375);
  }
}
 
 
// monadic lift
export function _mlift_main_10241(_y_x10121, _y_x10123, _y_x10125) /* (player, player, gtree) -> <div,exn,console/console> () */  {
   
  var x_10377 = perfect(function() {
    return cheat_report(function() {
      return check(function() {
        return $std_core_hnd._open_at1(2, alice_turn, 7);
      });
    });
  });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10129 /* player */ ) {
      return _mlift_main_10240(_y_x10121, _y_x10123, _y_x10125, _y_x10129);
    });
  }
  else {
    return _mlift_main_10240(_y_x10121, _y_x10123, _y_x10125, x_10377);
  }
}
 
 
// monadic lift
export function _mlift_main_10242(_y_x10121, _y_x10123) /* (player, player) -> <div,exn,console/console> () */  {
   
  var x_10379 = $std_core_hnd._open_none0(function() {
    return gametree(function() {
      return alice_turn(3);
    });
  });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10125 /* gtree */ ) {
      return _mlift_main_10241(_y_x10121, _y_x10123, _y_x10125);
    });
  }
  else {
    return _mlift_main_10241(_y_x10121, _y_x10123, x_10379);
  }
}
 
 
// monadic lift
export function _mlift_main_10243(_y_x10121) /* (player) -> <div,exn,console/console> () */  {
   
  var x_10381 = $std_core_hnd._open_none0(function() {
    return perfect(function() {
      return alice_turn(12);
    });
  });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10123 /* player */ ) {
      return _mlift_main_10242(_y_x10121, _y_x10123);
    });
  }
  else {
    return _mlift_main_10242(_y_x10121, x_10381);
  }
}
 
export function main() /* () -> <pure,console/console> () */  {
   
  var x_10383 = $std_core_hnd._open_none0(function() {
    return perfect(function() {
      return alice_turn(7);
    });
  });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_main_10243);
  }
  else {
     
    var x_0_10386 = $std_core_hnd._open_none0(function() {
      return perfect(function() {
        return alice_turn(12);
      });
    });
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10123 /* player */ ) {
        return _mlift_main_10242(x_10383, _y_x10123);
      });
    }
    else {
       
      var x_1_10389 = $std_core_hnd._open_none0(function() {
        return gametree(function() {
          return alice_turn(3);
        });
      });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10125 /* gtree */ ) {
          return _mlift_main_10241(x_10383, x_0_10386, _y_x10125);
        });
      }
      else {
         
        var x_2_10392 = perfect(function() {
          return cheat_report(function() {
            return check(function() {
              return $std_core_hnd._open_at1(2, alice_turn, 7);
            });
          });
        });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10129 /* player */ ) {
            return _mlift_main_10240(x_10383, x_0_10386, x_1_10389, _y_x10129);
          });
        }
        else {
           
          var x_3_10395 = perfect(function() {
            return cheat_report(function() {
              return check(function() {
                return pc(function() {
                  return $std_core_hnd._open_at1(2, alice_turn, 12);
                });
              });
            });
          });
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10134 /* player */ ) {
              return _mlift_main_10239(x_10383, x_0_10386, x_1_10389, x_2_10392, _y_x10134);
            });
          }
          else {
             
            var x_4_10398 = $std_core_list.show($std_core_hnd._open_none0(example_choose), function(_x_x1_1 /* player */ ) {
                return $std_core_hnd._open_none1(player_fs_show, _x_x1_1);
              });
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(function(_y_x10135 /* string */ ) {
                return _mlift_main_10238(x_10383, x_0_10386, x_1_10389, x_2_10392, x_3_10395, _y_x10135);
              });
            }
            else {
               
              $std_core_console.prints($std_core_list.joinsep($std_core_types.Cons("", $std_core_types.Cons($std_core_hnd._open_none1(function(p /* player */ ) {
                        return (p === 1) ? "bob" : "alice";
                      }, x_10383), $std_core_types.Cons($std_core_hnd._open_none1(function(p_0 /* player */ ) {
                          return (p_0 === 1) ? "bob" : "alice";
                        }, x_0_10386), $std_core_types.Cons($std_core_hnd._open_none1(function(gt /* gtree */ ) {
                            return show_gtree(gt, 2);
                          }, x_1_10389), $std_core_types.Cons($std_core_hnd._open_none1(function(p_1 /* player */ ) {
                              return (p_1 === 1) ? "bob" : "alice";
                            }, x_2_10392), $std_core_types.Cons($std_core_hnd._open_none1(function(p_2 /* player */ ) {
                                return (p_2 === 1) ? "bob" : "alice";
                              }, x_3_10395), $std_core_types.Cons(x_4_10398, $std_core_types.Cons("", $std_core_types.Nil)))))))), "\n-----------\n"));
              return $std_core_exn.exn_fs__handle($std_core_exn._Hnd_exn(0, function(m /* hnd/marker<<console/console,div,exn>,()> */ , ___wildcard_x654__16 /* hnd/ev<exn> */ , x_5 /* exception */ ) {
                    return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<10004,()>) -> <console/console,div,exn> () */ ) {
                        return $std_core_console.printsln($std_core_hnd._open_none1(function(exn_0 /* exception */ ) {
                            return exn_0.message;
                          }, x_5));
                      });
                  }), function(_res /* () */ ) {
                  return _res;
                }, function() {
                   
                  var x_6_10402 = $std_core_hnd._open_at0(0, function() {
                      return pc(function() {
                        return cheat_report(function() {
                          return check(function() {
                            return $std_core_hnd._open_at1(2, alice_turn, 12);
                          });
                        });
                      });
                    });
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(_mlift_main_10237);
                  }
                  else {
                    return $std_core_console.printsln($std_core_hnd._open_none1(function(p_3 /* player */ ) {
                        return (p_3 === 1) ? "bob" : "alice";
                      }, x_6_10402));
                  }
                });
            }
          }
        }
      }
    }
  }
}