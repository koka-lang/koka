// Koka generated module: "std/os/path", koka version: 2.3.2, platform: 64-bit
#include "std_os_path.h"

kk_std_os_path__path kk_std_os_path__copy(kk_std_os_path__path _this, kk_std_core_types__optional root0, kk_std_core_types__optional parts0, kk_context_t* _ctx) { /* (path, root : optional<string>, parts : optional<list<string>>) -> path */ 
  kk_string_t _x2670;
  if (kk_std_core_types__is_Optional(root0)) {
    kk_box_t _box_x2274 = root0._cons.Optional.value;
    kk_string_t _root_105 = kk_string_unbox(_box_x2274);
    kk_string_dup(_root_105);
    kk_std_core_types__optional_drop(root0, _ctx);
    _x2670 = _root_105; /*string*/
    goto _match2671;
  }
  {
    kk_string_t _x = _this.root;
    kk_string_dup(_x);
    _x2670 = _x; /*string*/
  }
  _match2671: ;
  kk_std_core__list _x2673;
  if (kk_std_core_types__is_Optional(parts0)) {
    kk_box_t _box_x2275 = parts0._cons.Optional.value;
    kk_std_core__list _parts_111 = kk_std_core__list_unbox(_box_x2275, NULL);
    kk_std_core__list_dup(_parts_111);
    kk_std_core_types__optional_drop(parts0, _ctx);
    kk_std_os_path__path_drop(_this, _ctx);
    _x2673 = _parts_111; /*list<string>*/
    goto _match2674;
  }
  {
    kk_std_core__list _x0 = _this.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(_this, _ctx);
    _x2673 = _x0; /*list<string>*/
  }
  _match2674: ;
  return kk_std_os_path__new_Path(_x2670, _x2673, _ctx);
}

kk_string_t kk_std_os_path_xapp_path(kk_context_t* _ctx) { /* () -> io string */ 
  return kk_os_app_path(kk_context());
}
 
// Return the base name of a path (stem name + extension)
// `"/foo/bar.txt".path.basename === "bar.txt"`
// `"/foo".path.basename === "foo"`

kk_string_t kk_std_os_path_basename(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  {
    kk_std_core__list _x = p.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con2676 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2276 = _con2676->head;
      kk_std_core__list _pat0 = _con2676->tail;
      kk_string_t x = kk_string_unbox(_box_x2276);
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_std_core__list_drop(_pat0, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x, _ctx);
      }
      return x;
    }
    {
      return kk_string_empty();
    }
  }
}
 
// Remove the basename and only keep the root and directory name portion of the path.
// `nobase("foo/bar.ext".path) == "foo")`

kk_std_os_path__path kk_std_os_path_nobase(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> path */ 
  kk_string_t _x2679;
  kk_std_core_types__optional _match_2669 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_2669)) {
    kk_box_t _box_x2277 = _match_2669._cons.Optional.value;
    kk_string_t _root_105 = kk_string_unbox(_box_x2277);
    kk_string_dup(_root_105);
    kk_std_core_types__optional_drop(_match_2669, _ctx);
    _x2679 = _root_105; /*string*/
    goto _match2680;
  }
  {
    kk_string_t _x0 = p.root;
    kk_string_dup(_x0);
    _x2679 = _x0; /*string*/
  }
  _match2680: ;
  kk_std_core__list _x2682;
  {
    kk_std_core__list _x = p.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con2684 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2278 = _con2684->head;
      kk_std_core__list xx = _con2684->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_box_drop(_box_x2278, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x, _ctx);
      }
      _x2682 = xx; /*list<string>*/
      goto _match2683;
    }
    {
      _x2682 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match2683: ;
  }
  return kk_std_os_path__new_Path(_x2679, _x2682, _ctx);
}

kk_std_core_types__tuple2_ kk_std_os_path_split_parts(kk_std_core__list parts0, kk_context_t* _ctx) { /* (parts : list<string>) -> (string, list<string>) */ 
  kk_box_t _x2686;
  kk_string_t _x2687;
  if (kk_std_core__is_Cons(parts0)) {
    struct kk_std_core_Cons* _con2689 = kk_std_core__as_Cons(parts0);
    kk_box_t _box_x2279 = _con2689->head;
    kk_string_t x = kk_string_unbox(_box_x2279);
    kk_string_dup(x);
    _x2687 = x; /*string*/
    goto _match2688;
  }
  {
    _x2687 = kk_string_empty(); /*string*/
  }
  _match2688: ;
  _x2686 = kk_string_box(_x2687); /*1004*/
  kk_box_t _x2692;
  kk_std_core__list _x2693;
  if (kk_std_core__is_Cons(parts0)) {
    struct kk_std_core_Cons* _con2695 = kk_std_core__as_Cons(parts0);
    kk_box_t _box_x2280 = _con2695->head;
    kk_std_core__list xx = _con2695->tail;
    if (kk_likely(kk_std_core__list_is_unique(parts0))) {
      kk_box_drop(_box_x2280, _ctx);
      kk_std_core__list_free(parts0);
    }
    else {
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(parts0, _ctx);
    }
    _x2693 = xx; /*list<string>*/
    goto _match2694;
  }
  {
    _x2693 = kk_std_core__new_Nil(_ctx); /*list<string>*/
  }
  _match2694: ;
  _x2692 = kk_std_core__list_box(_x2693, _ctx); /*1005*/
  return kk_std_core_types__new_dash__lp__comma__rp_(_x2686, _x2692, _ctx);
}

kk_string_t kk_std_os_path_xrealpath(kk_string_t p, kk_context_t* _ctx) { /* (p : string) -> io string */ 
  return kk_os_realpath(p,kk_context());
}
 
// Return the directory part of a path (including the rootname)
// `"/foo/bar.txt".path.dirname === "/foo"`
// `"/foo".path.dirname === "/"`

kk_string_t kk_std_os_path_dirname(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  kk_string_t _x2697;
  {
    kk_string_t _x = p.root;
    kk_string_dup(_x);
    _x2697 = _x; /*string*/
  }
  kk_string_t _x2698;
  kk_std_core__list xs_2032;
  kk_std_core__list _x2699;
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x0)) {
      struct kk_std_core_Cons* _con2701 = kk_std_core__as_Cons(_x0);
      kk_box_t _box_x2285 = _con2701->head;
      kk_std_core__list xx = _con2701->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x0))) {
        kk_box_drop(_box_x2285, _ctx);
        kk_std_core__list_free(_x0);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x0, _ctx);
      }
      _x2699 = xx; /*list<string>*/
      goto _match2700;
    }
    {
      _x2699 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match2700: ;
  }
  xs_2032 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), _x2699, _ctx); /*list<string>*/
  if (kk_std_core__is_Nil(xs_2032)) {
    _x2698 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con2704 = kk_std_core__as_Cons(xs_2032);
    kk_box_t _box_x2286 = _con2704->head;
    kk_std_core__list xx0 = _con2704->tail;
    kk_string_t x = kk_string_unbox(_box_x2286);
    if (kk_likely(kk_std_core__list_is_unique(xs_2032))) {
      kk_std_core__list_free(xs_2032);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx0);
      kk_std_core__list_decref(xs_2032, _ctx);
    }
    kk_string_t _x2706;
    kk_define_string_literal(, _s2707, 1, "/")
    _x2706 = kk_string_dup(_s2707); /*string*/
    _x2698 = kk_std_core__lift16737_joinsep(_x2706, xx0, x, _ctx); /*string*/
  }
  return kk_std_core__lp__plus__plus__1_rp_(_x2697, _x2698, _ctx);
}

kk_string_t kk_std_os_path_xhomedir(kk_context_t* _ctx) { /* () -> io string */ 
  return kk_os_home_dir(kk_context());
}
 
// Remove the directory and root and only keep the base name (file name) portion of the path.
// `nodir("foo/bar.ext".path) === "bar.ext"`

kk_std_os_path__path kk_std_os_path_nodir(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> path */ 
  kk_std_core__list _b_2288_2287;
  kk_std_core__list _x2709;
  {
    kk_std_core__list _x1 = p.parts;
    kk_std_core__list_dup(_x1);
    _x2709 = _x1; /*list<string>*/
  }
  _b_2288_2287 = kk_std_core_take(_x2709, kk_integer_from_small(1), _ctx); /*list<string>*/
  kk_string_t _x2710 = kk_string_empty(); /*string*/
  kk_std_core__list _x2712;
  kk_std_core_types__optional _match_2668 = kk_std_core_types__new_Optional(kk_std_core__list_box(_b_2288_2287, _ctx), _ctx); /*optional<1035>*/;
  if (kk_std_core_types__is_Optional(_match_2668)) {
    kk_box_t _box_x2289 = _match_2668._cons.Optional.value;
    kk_std_core__list _parts_111 = kk_std_core__list_unbox(_box_x2289, NULL);
    kk_std_os_path__path_drop(p, _ctx);
    kk_std_core__list_dup(_parts_111);
    kk_std_core_types__optional_drop(_match_2668, _ctx);
    _x2712 = _parts_111; /*list<string>*/
    goto _match2713;
  }
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p, _ctx);
    _x2712 = _x0; /*list<string>*/
  }
  _match2713: ;
  return kk_std_os_path__new_Path(_x2710, _x2712, _ctx);
}
 
// Return the last directory component name (or the empty string).
// `"c:/foo/bar/tst.txt".path.parentname === "bar"

kk_string_t kk_std_os_path_parentname(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  {
    kk_std_core__list _x = p.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    kk_std_core__list _match_2667;
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con2716 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2290 = _con2716->head;
      kk_std_core__list xx = _con2716->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_box_drop(_box_x2290, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x, _ctx);
      }
      _match_2667 = xx; /*list<string>*/
      goto _match2715;
    }
    {
      _match_2667 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match2715: ;
    if (kk_std_core__is_Cons(_match_2667)) {
      struct kk_std_core_Cons* _con2718 = kk_std_core__as_Cons(_match_2667);
      kk_box_t _box_x2291 = _con2718->head;
      kk_std_core__list _pat0 = _con2718->tail;
      kk_string_t x = kk_string_unbox(_box_x2291);
      if (kk_likely(kk_std_core__list_is_unique(_match_2667))) {
        kk_std_core__list_drop(_pat0, _ctx);
        kk_std_core__list_free(_match_2667);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_match_2667, _ctx);
      }
      return x;
    }
    {
      return kk_string_empty();
    }
  }
}
 
// Return the OS specific directory separator (`"/"` or `"\\"`)

kk_string_t kk_std_os_path_partsep(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_os_dir_sep(kk_context());
}
 
// Return the OS specific path separator (`';'` or `':'`)

kk_string_t kk_std_os_path_pathsep(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_os_path_sep(kk_context());
}

kk_string_t kk_std_os_path_xtempdir(kk_context_t* _ctx) { /* () -> io string */ 
  return kk_os_temp_dir(kk_context());
}
 
// Is a path empty?

bool kk_std_os_path_is_empty(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> bool */ 
  bool _match_2666;
  kk_string_t _x2721;
  {
    kk_string_t _x = p.root;
    kk_string_dup(_x);
    _x2721 = _x; /*string*/
  }
  kk_string_t _x2722 = kk_string_empty(); /*string*/
  _match_2666 = kk_string_is_eq(_x2721,_x2722,kk_context()); /*bool*/
  if (_match_2666) {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Nil(_x0)) {
      return true;
    }
    {
      kk_std_core__list_drop(_x0, _ctx);
      return false;
    }
  }
  {
    kk_std_os_path__path_drop(p, _ctx);
    return false;
  }
}
 
// Return the first path if it is not empty, otherwise return the second one.

kk_std_os_path__path kk_std_os_path__lp__bar__bar__rp_(kk_std_os_path__path p1, kk_std_os_path__path p2, kk_context_t* _ctx) { /* (p1 : path, p2 : path) -> path */ 
  bool _match_2665;
  kk_string_t _x2724;
  {
    kk_string_t _x = p1.root;
    kk_string_dup(_x);
    _x2724 = _x; /*string*/
  }
  kk_string_t _x2725 = kk_string_empty(); /*string*/
  _match_2665 = kk_string_is_eq(_x2724,_x2725,kk_context()); /*bool*/
  if (_match_2665) {
    kk_std_core__list _x0 = p1.parts;
    kk_std_core__list_dup(_x0);
    if (kk_std_core__is_Nil(_x0)) {
      kk_std_os_path__path_drop(p1, _ctx);
      return p2;
    }
    {
      kk_std_core__list_drop(_x0, _ctx);
      kk_std_os_path__path_drop(p2, _ctx);
      return p1;
    }
  }
  {
    kk_std_os_path__path_drop(p2, _ctx);
    return p1;
  }
}

kk_std_core__list kk_std_os_path_push_part(kk_string_t dir, kk_std_core__list dirs, kk_context_t* _ctx) { /* (dir : string, dirs : list<string>) -> list<string> */ 
  bool _match_2662;
  kk_string_t _x2727 = kk_string_dup(dir); /*string*/
  kk_string_t _x2728;
  kk_define_string_literal(, _s2729, 1, ".")
  _x2728 = kk_string_dup(_s2729); /*string*/
  _match_2662 = kk_string_is_eq(_x2727,_x2728,kk_context()); /*bool*/
  if (_match_2662) {
    kk_string_drop(dir, _ctx);
    return dirs;
  }
  {
    bool _match_2663;
    kk_string_t _x2730 = kk_string_dup(dir); /*string*/
    kk_string_t _x2731 = kk_string_empty(); /*string*/
    _match_2663 = kk_string_is_eq(_x2730,_x2731,kk_context()); /*bool*/
    if (_match_2663) {
      kk_string_drop(dir, _ctx);
      return dirs;
    }
    {
      bool _match_2664;
      kk_string_t _x2733 = kk_string_dup(dir); /*string*/
      kk_string_t _x2734;
      kk_define_string_literal(, _s2735, 2, "..")
      _x2734 = kk_string_dup(_s2735); /*string*/
      _match_2664 = kk_string_is_eq(_x2733,_x2734,kk_context()); /*bool*/
      if (_match_2664) {
        if (kk_std_core__is_Cons(dirs)) {
          struct kk_std_core_Cons* _con2736 = kk_std_core__as_Cons(dirs);
          kk_box_t _box_x2292 = _con2736->head;
          kk_string_drop(dir, _ctx);
          if (kk_std_core__is_Cons(dirs)) {
            struct kk_std_core_Cons* _con2738 = kk_std_core__as_Cons(dirs);
            kk_box_t _box_x2293 = _con2738->head;
            kk_std_core__list xx = _con2738->tail;
            if (kk_likely(kk_std_core__list_is_unique(dirs))) {
              kk_box_drop(_box_x2293, _ctx);
              kk_std_core__list_free(dirs);
            }
            else {
              kk_std_core__list_dup(xx);
              kk_std_core__list_decref(dirs, _ctx);
            }
            return xx;
          }
          {
            return kk_std_core__new_Nil(_ctx);
          }
        }
        {
          return kk_std_core__new_Cons(kk_reuse_null, kk_string_box(dir), dirs, _ctx);
        }
      }
      {
        return kk_std_core__new_Cons(kk_reuse_null, kk_string_box(dir), dirs, _ctx);
      }
    }
  }
}

kk_std_core__list kk_std_os_path_push_parts(kk_std_core__list parts0, kk_std_core__list dirs, kk_context_t* _ctx) { /* (parts : list<string>, dirs : list<string>) -> list<string> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(parts0)) {
    struct kk_std_core_Cons* _con2740 = kk_std_core__as_Cons(parts0);
    kk_box_t _box_x2302 = _con2740->head;
    kk_std_core__list rest = _con2740->tail;
    kk_string_t part = kk_string_unbox(_box_x2302);
    if (kk_likely(kk_std_core__list_is_unique(parts0))) {
      kk_std_core__list_free(parts0);
    }
    else {
      kk_string_dup(part);
      kk_std_core__list_dup(rest);
      kk_std_core__list_decref(parts0, _ctx);
    }
    { // tailcall
      kk_std_core__list _x2742 = kk_std_os_path_push_part(part, dirs, _ctx); /*list<string>*/
      parts0 = rest;
      dirs = _x2742;
      goto kk__tailcall;
    }
  }
  {
    return dirs;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2206_proot_fun2744__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2206_proot_fun2744(kk_function_t _fself, kk_box_t _b_2304, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2206_proot_fun2744(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2206_proot_fun2744, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2206_proot_fun2744(kk_function_t _fself, kk_box_t _b_2304, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2745 = kk_Unit;
  kk_char_t _x2746 = kk_char_unbox(_b_2304, _ctx); /*char*/
  kk_std_os_path__mlift2205_proot(_x2746, _ctx);
  return kk_unit_box(_x2745);
}

kk_unit_t kk_std_os_path__mlift2206_proot(kk_char_t wild__, kk_context_t* _ctx) { /* (wild_ : char) -> std/text/parse/parse () */ 
  kk_char_t x_2218 = kk_std_text_parse_char(':', _ctx); /*char*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x2743 = kk_std_core_hnd_yield_extend(kk_std_os_path__new_mlift2206_proot_fun2744(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x2743); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2208_proot_fun2747__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2208_proot_fun2747(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2208_proot_fun2747(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2208_proot_fun2747, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2208_proot_fun2747(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_char_t _x2748;
  kk_string_t _x2749;
  kk_define_string_literal(, _s2750, 1, "/")
  _x2749 = kk_string_dup(_s2750); /*string*/
  _x2748 = kk_std_text_parse_none_of(_x2749, _ctx); /*char*/
  return kk_char_box(_x2748, _ctx);
}


// lift anonymous function
struct kk_std_os_path__mlift2208_proot_fun2752__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2208_proot_fun2752(kk_function_t _fself, kk_box_t _b_2311, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2208_proot_fun2752(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2208_proot_fun2752, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2208_proot_fun2752(kk_function_t _fself, kk_box_t _b_2311, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2753 = kk_Unit;
  kk_std_core__list _x2754 = kk_std_core__list_unbox(_b_2311, _ctx); /*list<char>*/
  kk_std_os_path__mlift2207_proot(_x2754, _ctx);
  return kk_unit_box(_x2753);
}

kk_unit_t kk_std_os_path__mlift2208_proot(kk_char_t _y_2170, kk_context_t* _ctx) { /* (char) -> std/text/parse/parse () */ 
  kk_std_core__list x_2220 = kk_std_text_parse_many_acc(kk_std_os_path__new_mlift2208_proot_fun2747(_ctx), kk_std_core__new_Nil(_ctx), _ctx); /*list<char>*/;
  kk_std_core__list_drop(x_2220, _ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x2751 = kk_std_core_hnd_yield_extend(kk_std_os_path__new_mlift2208_proot_fun2752(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x2751); return kk_Unit;
  }
  {
    kk_Unit; return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2209_proot_fun2758__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2209_proot_fun2758(kk_function_t _fself, kk_box_t _b_2314, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2209_proot_fun2758(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2209_proot_fun2758, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2209_proot_fun2758(kk_function_t _fself, kk_box_t _b_2314, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2759 = kk_Unit;
  kk_char_t _x2760 = kk_char_unbox(_b_2314, _ctx); /*char*/
  kk_std_os_path__mlift2208_proot(_x2760, _ctx);
  return kk_unit_box(_x2759);
}

kk_unit_t kk_std_os_path__mlift2209_proot(kk_char_t wild__1, kk_context_t* _ctx) { /* (wild_1 : char) -> std/text/parse/parse () */ 
  kk_char_t x_2222;
  kk_string_t _x2755;
  kk_define_string_literal(, _s2756, 1, "/")
  _x2755 = kk_string_dup(_s2756); /*string*/
  x_2222 = kk_std_text_parse_none_of(_x2755, _ctx); /*char*/
  if (kk_yielding(kk_context())) {
    kk_box_t _x2757 = kk_std_core_hnd_yield_extend(kk_std_os_path__new_mlift2209_proot_fun2758(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x2757); return kk_Unit;
  }
  {
    kk_std_os_path__mlift2208_proot(x_2222, _ctx); return kk_Unit;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2210_proot_fun2762__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2210_proot_fun2762(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2210_proot_fun2762(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2210_proot_fun2762, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_os_path__mlift2210_proot_fun2765__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2210_proot_fun2765(kk_function_t _fself, kk_box_t _b_2317, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2210_proot_fun2765(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2210_proot_fun2765, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2210_proot_fun2765(kk_function_t _fself, kk_box_t _b_2317, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2766;
  kk_char_t _x2767 = kk_char_unbox(_b_2317, _ctx); /*char*/
  _x2766 = kk_std_os_path__mlift2203_proot(_x2767, _ctx); /*bool*/
  return kk_bool_box(_x2766);
}
static kk_box_t kk_std_os_path__mlift2210_proot_fun2762(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2763;
  kk_char_t x_2224 = kk_std_text_parse_char('/', _ctx); /*char*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x2764 = kk_std_core_hnd_yield_extend(kk_std_os_path__new_mlift2210_proot_fun2765(_ctx), _ctx); /*1002*/
    _x2763 = kk_bool_unbox(_x2764); /*bool*/
  }
  else {
    _x2763 = false; /*bool*/
  }
  return kk_bool_box(_x2763);
}


// lift anonymous function
struct kk_std_os_path__mlift2210_proot_fun2768__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2210_proot_fun2768(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2210_proot_fun2768(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2210_proot_fun2768, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_os_path__mlift2210_proot_fun2771__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2210_proot_fun2771(kk_function_t _fself, kk_box_t _b_2319, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2210_proot_fun2771(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2210_proot_fun2771, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2210_proot_fun2771(kk_function_t _fself, kk_box_t _b_2319, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2772;
  kk_unit_t _x2773 = kk_Unit;
  kk_unit_unbox(_b_2319);
  _x2772 = kk_std_os_path__mlift2204_proot(_x2773, _ctx); /*bool*/
  return kk_bool_box(_x2772);
}
static kk_box_t kk_std_os_path__mlift2210_proot_fun2768(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2769;
  kk_unit_t x0_2226 = kk_Unit;
  kk_std_text_parse_eof(_ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x2770 = kk_std_core_hnd_yield_extend(kk_std_os_path__new_mlift2210_proot_fun2771(_ctx), _ctx); /*1002*/
    _x2769 = kk_bool_unbox(_x2770); /*bool*/
  }
  else {
    _x2769 = true; /*bool*/
  }
  return kk_bool_box(_x2769);
}

bool kk_std_os_path__mlift2210_proot(kk_unit_t wild__3, kk_context_t* _ctx) { /* (wild_3 : ()) -> std/text/parse/parse bool */ 
  kk_box_t _x2761 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_os_path__new_mlift2210_proot_fun2762(_ctx), kk_std_os_path__new_mlift2210_proot_fun2768(_ctx), _ctx); /*948*/
  return kk_bool_unbox(_x2761);
}


// lift anonymous function
struct kk_std_os_path_proot_fun2775__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2775(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2775(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2775, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_os_path_proot_fun2778__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2778(kk_function_t _fself, kk_box_t _b_2327, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2778(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2778, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_proot_fun2778(kk_function_t _fself, kk_box_t _b_2327, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2779 = kk_Unit;
  kk_char_t _x2780 = kk_char_unbox(_b_2327, _ctx); /*char*/
  kk_std_os_path__mlift2206_proot(_x2780, _ctx);
  return kk_unit_box(_x2779);
}
static kk_box_t kk_std_os_path_proot_fun2775(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2776 = kk_Unit;
  kk_char_t x0_2231 = kk_std_text_parse_alpha(_ctx); /*char*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x2777 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_proot_fun2778(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x2777);
  }
  else {
    kk_std_os_path__mlift2206_proot(x0_2231, _ctx);
  }
  return kk_unit_box(_x2776);
}


// lift anonymous function
struct kk_std_os_path_proot_fun2781__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2781(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2781(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2781, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_os_path_proot_fun2784__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2784(kk_function_t _fself, kk_box_t _b_2329, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2784(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2784, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_proot_fun2784(kk_function_t _fself, kk_box_t _b_2329, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2785 = kk_Unit;
  kk_char_t _x2786 = kk_char_unbox(_b_2329, _ctx); /*char*/
  kk_std_os_path__mlift2209_proot(_x2786, _ctx);
  return kk_unit_box(_x2785);
}
static kk_box_t kk_std_os_path_proot_fun2781(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_unit_t _x2782 = kk_Unit;
  kk_char_t x1_2233 = kk_std_text_parse_char('/', _ctx); /*char*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x2783 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_proot_fun2784(_ctx), _ctx); /*1002*/
    kk_unit_unbox(_x2783);
  }
  else {
    kk_std_os_path__mlift2209_proot(x1_2233, _ctx);
  }
  return kk_unit_box(_x2782);
}


// lift anonymous function
struct kk_std_os_path_proot_fun2788__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2788(kk_function_t _fself, kk_box_t _b_2337, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2788(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2788, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_proot_fun2788(kk_function_t _fself, kk_box_t _b_2337, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2789;
  kk_unit_t _x2790 = kk_Unit;
  kk_unit_unbox(_b_2337);
  _x2789 = kk_std_os_path__mlift2210_proot(_x2790, _ctx); /*bool*/
  return kk_bool_box(_x2789);
}


// lift anonymous function
struct kk_std_os_path_proot_fun2791__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2791(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2791(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2791, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_os_path_proot_fun2794__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2794(kk_function_t _fself, kk_box_t _b_2339, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2794(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2794, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_proot_fun2794(kk_function_t _fself, kk_box_t _b_2339, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2795;
  kk_char_t _x2796 = kk_char_unbox(_b_2339, _ctx); /*char*/
  _x2795 = kk_std_os_path__mlift2203_proot(_x2796, _ctx); /*bool*/
  return kk_bool_box(_x2795);
}
static kk_box_t kk_std_os_path_proot_fun2791(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2792;
  kk_char_t x2_2235 = kk_std_text_parse_char('/', _ctx); /*char*/;
  if (kk_yielding(kk_context())) {
    kk_box_t _x2793 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_proot_fun2794(_ctx), _ctx); /*1002*/
    _x2792 = kk_bool_unbox(_x2793); /*bool*/
  }
  else {
    _x2792 = false; /*bool*/
  }
  return kk_bool_box(_x2792);
}


// lift anonymous function
struct kk_std_os_path_proot_fun2797__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2797(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2797(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2797, _ctx)
  return kk_function_dup(_fself);
}



// lift anonymous function
struct kk_std_os_path_proot_fun2800__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_proot_fun2800(kk_function_t _fself, kk_box_t _b_2341, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_proot_fun2800(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_proot_fun2800, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_proot_fun2800(kk_function_t _fself, kk_box_t _b_2341, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2801;
  kk_unit_t _x2802 = kk_Unit;
  kk_unit_unbox(_b_2341);
  _x2801 = kk_std_os_path__mlift2204_proot(_x2802, _ctx); /*bool*/
  return kk_bool_box(_x2801);
}
static kk_box_t kk_std_os_path_proot_fun2797(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2798;
  kk_unit_t x3_2237 = kk_Unit;
  kk_std_text_parse_eof(_ctx);
  if (kk_yielding(kk_context())) {
    kk_box_t _x2799 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_proot_fun2800(_ctx), _ctx); /*1002*/
    _x2798 = kk_bool_unbox(_x2799); /*bool*/
  }
  else {
    _x2798 = true; /*bool*/
  }
  return kk_bool_box(_x2798);
}

bool kk_std_os_path_proot(kk_context_t* _ctx) { /* () -> std/text/parse/parse bool */ 
  kk_unit_t x_2228 = kk_Unit;
  kk_box_t _x2774 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_os_path_new_proot_fun2775(_ctx), kk_std_os_path_new_proot_fun2781(_ctx), _ctx); /*948*/
  kk_unit_unbox(_x2774);
  kk_box_t _x2787;
  if (kk_yielding(kk_context())) {
    _x2787 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_proot_fun2788(_ctx), _ctx); /*1002*/
  }
  else {
    _x2787 = kk_std_text_parse__lp__bar__bar__rp_(kk_std_os_path_new_proot_fun2791(_ctx), kk_std_os_path_new_proot_fun2797(_ctx), _ctx); /*1002*/
  }
  return kk_bool_unbox(_x2787);
}
 
// Convert a `:path` to a normalized `:string` path.
// If this results in an empty string, the current directory path `"."` is returned.
// `"c:/foo/test.txt".path.string -> "c:/foo/test.txt"`
// `"c:\\foo\\test.txt".path.string -> "c:/foo/test.txt"`
// `"/foo//./bar/../test.txt".path.string -> "/foo/test.txt"`

kk_string_t kk_std_os_path_string(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  kk_string_t s;
  kk_string_t _x2803;
  {
    kk_string_t _x = p.root;
    kk_string_dup(_x);
    _x2803 = _x; /*string*/
  }
  kk_string_t _x2804;
  kk_std_core__list xs_2058;
  kk_std_core__list _x2805;
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p, _ctx);
    _x2805 = _x0; /*list<string>*/
  }
  xs_2058 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), _x2805, _ctx); /*list<string>*/
  if (kk_std_core__is_Nil(xs_2058)) {
    _x2804 = kk_string_empty(); /*string*/
  }
  else {
    struct kk_std_core_Cons* _con2807 = kk_std_core__as_Cons(xs_2058);
    kk_box_t _box_x2349 = _con2807->head;
    kk_std_core__list xx = _con2807->tail;
    kk_string_t x = kk_string_unbox(_box_x2349);
    if (kk_likely(kk_std_core__list_is_unique(xs_2058))) {
      kk_std_core__list_free(xs_2058);
    }
    else {
      kk_string_dup(x);
      kk_std_core__list_dup(xx);
      kk_std_core__list_decref(xs_2058, _ctx);
    }
    kk_string_t _x2809;
    kk_define_string_literal(, _s2810, 1, "/")
    _x2809 = kk_string_dup(_s2810); /*string*/
    _x2804 = kk_std_core__lift16737_joinsep(_x2809, xx, x, _ctx); /*string*/
  }
  s = kk_std_core__lp__plus__plus__1_rp_(_x2803, _x2804, _ctx); /*string*/
  bool _match_2651;
  kk_string_t _x2811 = kk_string_dup(s); /*string*/
  kk_string_t _x2812 = kk_string_empty(); /*string*/
  _match_2651 = kk_string_is_eq(_x2811,_x2812,kk_context()); /*bool*/
  if (_match_2651) {
    kk_string_drop(s, _ctx);
    kk_define_string_literal(, _s2814, 1, ".")
    return kk_string_dup(_s2814);
  }
  {
    return s;
  }
}

kk_std_os_path__path kk_std_os_path_path_parts(kk_string_t root0, kk_string_t s, kk_std_core_types__optional dirs, kk_context_t* _ctx) { /* (root : string, s : string, dirs : optional<list<string>>) -> path */ 
  kk_std_core__list parts0;
  kk_std_core__list _x2822;
  kk_vector_t v_16666;
  kk_string_t _x2823;
  kk_define_string_literal(, _s2824, 1, "/")
  _x2823 = kk_string_dup(_s2824); /*string*/
  v_16666 = kk_string_splitv(s,_x2823,kk_context()); /*vector<string>*/
  _x2822 = kk_std_core_vlist(v_16666, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
  kk_std_core__list _x2825;
  if (kk_std_core_types__is_Optional(dirs)) {
    kk_box_t _box_x2352 = dirs._cons.Optional.value;
    kk_std_core__list _dirs_1011 = kk_std_core__list_unbox(_box_x2352, NULL);
    kk_std_core__list_dup(_dirs_1011);
    kk_std_core_types__optional_drop(dirs, _ctx);
    _x2825 = _dirs_1011; /*list<string>*/
    goto _match2826;
  }
  {
    _x2825 = kk_std_core__new_Nil(_ctx); /*list<string>*/
  }
  _match2826: ;
  parts0 = kk_std_os_path_push_parts(_x2822, _x2825, _ctx); /*list<string>*/
  return kk_std_os_path__new_Path(root0, parts0, _ctx);
}
 
// Create a normalized `:path` from a path string.


// lift anonymous function
struct kk_std_os_path_path_fun2838__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_path_fun2838(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_path_fun2838(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_path_fun2838, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_path_fun2838(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  bool _x2839 = kk_std_os_path_proot(_ctx); /*bool*/
  return kk_bool_box(_x2839);
}

kk_std_os_path__path kk_std_os_path_path(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> path */ 
  bool _match_2647;
  kk_string_t _x2828 = kk_string_dup(s); /*string*/
  kk_string_t _x2829 = kk_string_empty(); /*string*/
  _match_2647 = kk_string_is_eq(_x2828,_x2829,kk_context()); /*bool*/
  if (_match_2647) {
    kk_string_drop(s, _ctx);
    kk_string_t _x2831 = kk_string_empty(); /*string*/
    return kk_std_os_path__new_Path(_x2831, kk_std_core__new_Nil(_ctx), _ctx);
  }
  {
    kk_string_t t;
    kk_string_t _x2833;
    kk_define_string_literal(, _s2834, 1, "\\")
    _x2833 = kk_string_dup(_s2834); /*string*/
    kk_string_t _x2835;
    kk_define_string_literal(, _s2836, 1, "/")
    _x2835 = kk_string_dup(_s2836); /*string*/
    t = kk_string_replace_all(s,_x2833,_x2835,kk_context()); /*string*/
    kk_std_core_types__maybe _match_2648;
    kk_string_t _x2837 = kk_string_dup(t); /*string*/
    _match_2648 = kk_std_text_parse_starts_with(_x2837, kk_std_os_path_new_path_fun2838(_ctx), _ctx); /*maybe<(2210, sslice)>*/
    if (kk_std_core_types__is_Nothing(_match_2648)) {
      kk_vector_t v_16666;
      kk_string_t _x2840;
      kk_define_string_literal(, _s2841, 1, "/")
      _x2840 = kk_string_dup(_s2841); /*string*/
      v_16666 = kk_string_splitv(t,_x2840,kk_context()); /*vector<string>*/
      kk_std_core__list parts0;
      kk_std_core__list _x2842 = kk_std_core_vlist(v_16666, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
      kk_std_core__list _x2843;
      kk_std_core_types__optional _match_2650 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
      if (kk_std_core_types__is_Optional(_match_2650)) {
        kk_box_t _box_x2355 = _match_2650._cons.Optional.value;
        kk_std_core__list _dirs_1011 = kk_std_core__list_unbox(_box_x2355, NULL);
        kk_std_core__list_dup(_dirs_1011);
        kk_std_core_types__optional_drop(_match_2650, _ctx);
        _x2843 = _dirs_1011; /*list<string>*/
        goto _match2844;
      }
      {
        _x2843 = kk_std_core__new_Nil(_ctx); /*list<string>*/
      }
      _match2844: ;
      parts0 = kk_std_os_path_push_parts(_x2842, _x2843, _ctx); /*list<string>*/
      kk_string_t _x2846 = kk_string_empty(); /*string*/
      return kk_std_os_path__new_Path(_x2846, parts0, _ctx);
    }
    {
      kk_box_t _box_x2356 = _match_2648._cons.Just.value;
      kk_std_core_types__tuple2_ _pat3 = kk_std_core_types__tuple2__unbox(_box_x2356, NULL);
      kk_box_t _box_x2357 = _pat3.fst;
      kk_box_t _box_x2358 = _pat3.snd;
      bool eof = kk_bool_unbox(_box_x2357);
      kk_std_core__sslice rest = kk_std_core__sslice_unbox(_box_x2358, NULL);
      kk_string_drop(t, _ctx);
      kk_std_core__sslice_dup(rest);
      kk_std_core_types__maybe_drop(_match_2648, _ctx);
      kk_string_t root1_2071;
      kk_string_t _x2851;
      kk_std_core__sslice _x2852;
      {
        kk_string_t s5 = rest.str;
        kk_ssize_t start0 = rest.start;
        kk_string_dup(s5);
        kk_ssize_t _x2853 = ((kk_ssize_t)0); /*ssize_t*/
        _x2852 = kk_std_core__new_Sslice(s5, _x2853, start0, _ctx); /*sslice*/
      }
      _x2851 = kk_std_core_string_3(_x2852, _ctx); /*string*/
      kk_string_t _x2854;
      if (eof) {
        kk_define_string_literal(, _s2855, 1, "/")
        _x2854 = kk_string_dup(_s2855); /*string*/
      }
      else {
        _x2854 = kk_string_empty(); /*string*/
      }
      root1_2071 = kk_std_core__lp__plus__plus__1_rp_(_x2851, _x2854, _ctx); /*string*/
      kk_string_t s3_2072 = kk_std_core_string_3(rest, _ctx); /*string*/;
      kk_vector_t v_166660;
      kk_string_t _x2857;
      kk_define_string_literal(, _s2858, 1, "/")
      _x2857 = kk_string_dup(_s2858); /*string*/
      v_166660 = kk_string_splitv(s3_2072,_x2857,kk_context()); /*vector<string>*/
      kk_std_core__list parts1;
      kk_std_core__list _x2859 = kk_std_core_vlist(v_166660, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
      kk_std_core__list _x2860;
      kk_std_core_types__optional _match_2649 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
      if (kk_std_core_types__is_Optional(_match_2649)) {
        kk_box_t _box_x2359 = _match_2649._cons.Optional.value;
        kk_std_core__list _dirs_10110 = kk_std_core__list_unbox(_box_x2359, NULL);
        kk_std_core__list_dup(_dirs_10110);
        kk_std_core_types__optional_drop(_match_2649, _ctx);
        _x2860 = _dirs_10110; /*list<string>*/
        goto _match2861;
      }
      {
        _x2860 = kk_std_core__new_Nil(_ctx); /*list<string>*/
      }
      _match2861: ;
      parts1 = kk_std_os_path_push_parts(_x2859, _x2860, _ctx); /*list<string>*/
      return kk_std_os_path__new_Path(root1_2071, parts1, _ctx);
    }
  }
}
 
// Add two paths together using left-associative operator `(/)`.
// Keeps the root of `p1` and discards the root name of `p2`.
// `"/a/" / "b/foo.txt"          === "/a/b/foo.txt"`
// `"/a/foo.txt" / "/b/bar.txt"  === "/a/foo.txt/b/bar.txt"`
// `"c:/foo" / "d:/bar"          === "c:/foo/bar"`

kk_std_os_path__path kk_std_os_path__lp__fs__rp_(kk_std_os_path__path p1, kk_std_os_path__path p2, kk_context_t* _ctx) { /* (p1 : path, p2 : path) -> path */ 
  kk_std_core__list _b_2363_2362;
  kk_std_core__list _x2863;
  kk_std_core__list _x2864;
  {
    kk_std_core__list _x0 = p2.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p2, _ctx);
    _x2864 = _x0; /*list<string>*/
  }
  _x2863 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), _x2864, _ctx); /*list<1001>*/
  kk_std_core__list _x2865;
  {
    kk_std_core__list _x1 = p1.parts;
    kk_std_core__list_dup(_x1);
    _x2865 = _x1; /*list<string>*/
  }
  _b_2363_2362 = kk_std_os_path_push_parts(_x2863, _x2865, _ctx); /*list<string>*/
  kk_string_t _x2866;
  {
    kk_string_t _x = p1.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(p1, _ctx);
    _x2866 = _x; /*string*/
  }
  kk_std_core__list _x2867;
  kk_std_core_types__optional _match_2646 = kk_std_core_types__new_Optional(kk_std_core__list_box(_b_2363_2362, _ctx), _ctx); /*optional<1035>*/;
  if (kk_std_core_types__is_Optional(_match_2646)) {
    kk_box_t _box_x2364 = _match_2646._cons.Optional.value;
    kk_std_core__list _parts_876 = kk_std_core__list_unbox(_box_x2364, NULL);
    kk_std_core__list_dup(_parts_876);
    kk_std_core_types__optional_drop(_match_2646, _ctx);
    _x2867 = _parts_876; /*list<string>*/
    goto _match2868;
  }
  {
    _x2867 = kk_std_core__new_Nil(_ctx); /*list<string>*/
  }
  _match2868: ;
  return kk_std_os_path__new_Path(_x2866, _x2867, _ctx);
}
 
// Convenience function that adds a string path.

kk_std_os_path__path kk_std_os_path__lp__fs__1_rp_(kk_std_os_path__path p1, kk_string_t p2, kk_context_t* _ctx) { /* (p1 : path, p2 : string) -> path */ 
  kk_std_os_path__path p20_2144 = kk_std_os_path_path(p2, _ctx); /*std/os/path/path*/;
  kk_std_core__list _b_2366_2365;
  kk_std_core__list _x2870;
  kk_std_core__list _x2871;
  {
    kk_std_core__list _x0 = p20_2144.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p20_2144, _ctx);
    _x2871 = _x0; /*list<string>*/
  }
  _x2870 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), _x2871, _ctx); /*list<1001>*/
  kk_std_core__list _x2872;
  {
    kk_std_core__list _x1 = p1.parts;
    kk_std_core__list_dup(_x1);
    _x2872 = _x1; /*list<string>*/
  }
  _b_2366_2365 = kk_std_os_path_push_parts(_x2870, _x2872, _ctx); /*list<string>*/
  kk_string_t _x2873;
  {
    kk_string_t _x = p1.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(p1, _ctx);
    _x2873 = _x; /*string*/
  }
  kk_std_core__list _x2874;
  kk_std_core_types__optional _match_2645 = kk_std_core_types__new_Optional(kk_std_core__list_box(_b_2366_2365, _ctx), _ctx); /*optional<1035>*/;
  if (kk_std_core_types__is_Optional(_match_2645)) {
    kk_box_t _box_x2367 = _match_2645._cons.Optional.value;
    kk_std_core__list _parts_876 = kk_std_core__list_unbox(_box_x2367, NULL);
    kk_std_core__list_dup(_parts_876);
    kk_std_core_types__optional_drop(_match_2645, _ctx);
    _x2874 = _parts_876; /*list<string>*/
    goto _match2875;
  }
  {
    _x2874 = kk_std_core__new_Nil(_ctx); /*list<string>*/
  }
  _match2875: ;
  return kk_std_os_path__new_Path(_x2873, _x2874, _ctx);
}
 
// Convenience function that adds two strings into a path.

kk_std_os_path__path kk_std_os_path__lp__fs__2_rp_(kk_string_t p1, kk_string_t p2, kk_context_t* _ctx) { /* (p1 : string, p2 : string) -> path */ 
  kk_std_os_path__path p10_2145 = kk_std_os_path_path(p1, _ctx); /*std/os/path/path*/;
  kk_std_os_path__path p20_2146 = kk_std_os_path_path(p2, _ctx); /*std/os/path/path*/;
  kk_std_core__list _b_2369_2368;
  kk_std_core__list _x2877;
  kk_std_core__list _x2878;
  {
    kk_std_core__list _x0 = p20_2146.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p20_2146, _ctx);
    _x2878 = _x0; /*list<string>*/
  }
  _x2877 = kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), _x2878, _ctx); /*list<1001>*/
  kk_std_core__list _x2879;
  {
    kk_std_core__list _x1 = p10_2145.parts;
    kk_std_core__list_dup(_x1);
    _x2879 = _x1; /*list<string>*/
  }
  _b_2369_2368 = kk_std_os_path_push_parts(_x2877, _x2879, _ctx); /*list<string>*/
  kk_string_t _x2880;
  {
    kk_string_t _x = p10_2145.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(p10_2145, _ctx);
    _x2880 = _x; /*string*/
  }
  kk_std_core__list _x2881;
  kk_std_core_types__optional _match_2644 = kk_std_core_types__new_Optional(kk_std_core__list_box(_b_2369_2368, _ctx), _ctx); /*optional<1035>*/;
  if (kk_std_core_types__is_Optional(_match_2644)) {
    kk_box_t _box_x2370 = _match_2644._cons.Optional.value;
    kk_std_core__list _parts_876 = kk_std_core__list_unbox(_box_x2370, NULL);
    kk_std_core__list_dup(_parts_876);
    kk_std_core_types__optional_drop(_match_2644, _ctx);
    _x2881 = _parts_876; /*list<string>*/
    goto _match2882;
  }
  {
    _x2881 = kk_std_core__new_Nil(_ctx); /*list<string>*/
  }
  _match2882: ;
  return kk_std_os_path__new_Path(_x2880, _x2881, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2211_app_path_fun2885__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2211_app_path_fun2885(kk_function_t _fself, kk_box_t _b_2373, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2211_app_path_fun2885(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2211_app_path_fun2885, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2211_app_path_fun2885(kk_function_t _fself, kk_box_t _b_2373, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2886;
  kk_string_t _x2887 = kk_string_unbox(_b_2373); /*string*/
  _x2886 = kk_std_os_path_path(_x2887, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2886, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2211_app_path(kk_string_t _y_2178, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_box_t _x2884 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2211_app_path_fun2885(_ctx), kk_string_box(_y_2178), _ctx); /*1002*/
  return kk_std_os_path__path_unbox(_x2884, _ctx);
}
 
// Return the path to the currently executing application.


// lift anonymous function
struct kk_std_os_path_app_path_fun2889__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_app_path_fun2889(kk_function_t _fself, kk_box_t _b_2377, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_app_path_fun2889(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_app_path_fun2889, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_app_path_fun2889(kk_function_t _fself, kk_box_t _b_2377, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2890;
  kk_string_t _x2891 = kk_string_unbox(_b_2377); /*string*/
  _x2890 = kk_std_os_path__mlift2211_app_path(_x2891, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2890, _ctx);
}


// lift anonymous function
struct kk_std_os_path_app_path_fun2892__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_app_path_fun2892(kk_function_t _fself, kk_box_t _b_2380, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_app_path_fun2892(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_app_path_fun2892, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_app_path_fun2892(kk_function_t _fself, kk_box_t _b_2380, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2893;
  kk_string_t _x2894 = kk_string_unbox(_b_2380); /*string*/
  _x2893 = kk_std_os_path_path(_x2894, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2893, _ctx);
}

kk_std_os_path__path kk_std_os_path_app_path(kk_context_t* _ctx) { /* () -> io path */ 
  kk_string_t x_2239 = kk_std_os_path_xapp_path(_ctx); /*string*/;
  kk_box_t _x2888;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2239, _ctx);
    _x2888 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_app_path_fun2889(_ctx), _ctx); /*1002*/
  }
  else {
    _x2888 = kk_std_core_hnd__open_none1(kk_std_os_path_new_app_path_fun2892(_ctx), kk_string_box(x_2239), _ctx); /*1002*/
  }
  return kk_std_os_path__path_unbox(_x2888, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2212_appdir_fun2896__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2212_appdir_fun2896(kk_function_t _fself, kk_box_t _b_2386, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2212_appdir_fun2896(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2212_appdir_fun2896, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2212_appdir_fun2896(kk_function_t _fself, kk_box_t _b_2386, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2897;
  kk_string_t _x2898 = kk_string_unbox(_b_2386); /*string*/
  _x2897 = kk_std_os_path_path(_x2898, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2897, _ctx);
}


// lift anonymous function
struct kk_std_os_path__mlift2212_appdir_fun2900__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2212_appdir_fun2900(kk_function_t _fself, kk_box_t _b_2393, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2212_appdir_fun2900(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2212_appdir_fun2900, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2212_appdir_fun2900(kk_function_t _fself, kk_box_t _b_2393, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2901;
  kk_string_t _x2902;
  kk_std_core_types__optional _match_2641 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_2641)) {
    kk_box_t _box_x2389 = _match_2641._cons.Optional.value;
    kk_string_t _root_105 = kk_string_unbox(_box_x2389);
    kk_string_dup(_root_105);
    kk_std_core_types__optional_drop(_match_2641, _ctx);
    _x2902 = _root_105; /*string*/
    goto _match2903;
  }
  {
    kk_std_os_path__path _match_2642;
    kk_box_t _x2905 = kk_box_dup(_b_2393); /*1001*/
    _match_2642 = kk_std_os_path__path_unbox(_x2905, _ctx); /*std/os/path/path*/
    {
      kk_string_t _x00 = _match_2642.root;
      kk_string_dup(_x00);
      kk_std_os_path__path_drop(_match_2642, _ctx);
      _x2902 = _x00; /*string*/
    }
  }
  _match2903: ;
  kk_std_core__list _x2906;
  kk_std_os_path__path _match_2640 = kk_std_os_path__path_unbox(_b_2393, _ctx); /*std/os/path/path*/;
  {
    kk_std_core__list _x = _match_2640.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(_match_2640, _ctx);
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con2908 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2390 = _con2908->head;
      kk_std_core__list xx = _con2908->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_box_drop(_box_x2390, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x, _ctx);
      }
      _x2906 = xx; /*list<string>*/
      goto _match2907;
    }
    {
      _x2906 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match2907: ;
  }
  _x2901 = kk_std_os_path__new_Path(_x2902, _x2906, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2901, _ctx);
}


// lift anonymous function
struct kk_std_os_path__mlift2212_appdir_fun2911__t {
  struct kk_function_s _base;
  kk_std_os_path__path p0;
};
static kk_box_t kk_std_os_path__mlift2212_appdir_fun2911(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2212_appdir_fun2911(kk_std_os_path__path p0, kk_context_t* _ctx) {
  struct kk_std_os_path__mlift2212_appdir_fun2911__t* _self = kk_function_alloc_as(struct kk_std_os_path__mlift2212_appdir_fun2911__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_os_path__mlift2212_appdir_fun2911, kk_context());
  _self->p0 = p0;
  return &_self->_base;
}

static kk_box_t kk_std_os_path__mlift2212_appdir_fun2911(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_os_path__mlift2212_appdir_fun2911__t* _self = kk_function_as(struct kk_std_os_path__mlift2212_appdir_fun2911__t*, _fself);
  kk_std_os_path__path p0 = _self->p0; /* std/os/path/path */
  kk_drop_match(_self, {kk_std_os_path__path_dup(p0);}, {}, _ctx)
  kk_std_os_path__path _x2912;
  bool _match_2636;
  kk_string_t _x2913;
  {
    kk_std_core__list _x11 = p0.parts;
    kk_std_core__list_dup(_x11);
    if (kk_std_core__is_Cons(_x11)) {
      struct kk_std_core_Cons* _con2915 = kk_std_core__as_Cons(_x11);
      kk_box_t _box_x2397 = _con2915->head;
      kk_std_core__list _pat03 = _con2915->tail;
      kk_string_t x = kk_string_unbox(_box_x2397);
      if (kk_likely(kk_std_core__list_is_unique(_x11))) {
        kk_std_core__list_drop(_pat03, _ctx);
        kk_std_core__list_free(_x11);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x11, _ctx);
      }
      _x2913 = x; /*string*/
      goto _match2914;
    }
    {
      _x2913 = kk_string_empty(); /*string*/
    }
    _match2914: ;
  }
  kk_string_t _x2918;
  kk_define_string_literal(, _s2919, 3, "bin")
  _x2918 = kk_string_dup(_s2919); /*string*/
  _match_2636 = kk_string_is_eq(_x2913,_x2918,kk_context()); /*bool*/
  if (_match_2636) {
    kk_string_t _x2920;
    kk_std_core_types__optional _match_2639 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_2639)) {
      kk_box_t _box_x2398 = _match_2639._cons.Optional.value;
      kk_string_t _root_1050 = kk_string_unbox(_box_x2398);
      kk_string_dup(_root_1050);
      kk_std_core_types__optional_drop(_match_2639, _ctx);
      _x2920 = _root_1050; /*string*/
      goto _match2921;
    }
    {
      kk_string_t _x000 = p0.root;
      kk_string_dup(_x000);
      _x2920 = _x000; /*string*/
    }
    _match2921: ;
    kk_std_core__list _x2923;
    {
      kk_std_core__list _x2 = p0.parts;
      kk_std_core__list_dup(_x2);
      kk_std_os_path__path_drop(p0, _ctx);
      if (kk_std_core__is_Cons(_x2)) {
        struct kk_std_core_Cons* _con2925 = kk_std_core__as_Cons(_x2);
        kk_box_t _box_x2399 = _con2925->head;
        kk_std_core__list xx0 = _con2925->tail;
        if (kk_likely(kk_std_core__list_is_unique(_x2))) {
          kk_box_drop(_box_x2399, _ctx);
          kk_std_core__list_free(_x2);
        }
        else {
          kk_std_core__list_dup(xx0);
          kk_std_core__list_decref(_x2, _ctx);
        }
        _x2923 = xx0; /*list<string>*/
        goto _match2924;
      }
      {
        _x2923 = kk_std_core__new_Nil(_ctx); /*list<string>*/
      }
      _match2924: ;
    }
    _x2912 = kk_std_os_path__new_Path(_x2920, _x2923, _ctx); /*std/os/path/path*/
  }
  else {
    bool _match_2637;
    kk_string_t _x2927;
    {
      kk_std_core__list _x010 = p0.parts;
      kk_std_core__list_dup(_x010);
      if (kk_std_core__is_Cons(_x010)) {
        struct kk_std_core_Cons* _con2929 = kk_std_core__as_Cons(_x010);
        kk_box_t _box_x2400 = _con2929->head;
        kk_std_core__list _pat021 = _con2929->tail;
        kk_string_t x0 = kk_string_unbox(_box_x2400);
        if (kk_likely(kk_std_core__list_is_unique(_x010))) {
          kk_std_core__list_drop(_pat021, _ctx);
          kk_std_core__list_free(_x010);
        }
        else {
          kk_string_dup(x0);
          kk_std_core__list_decref(_x010, _ctx);
        }
        _x2927 = x0; /*string*/
        goto _match2928;
      }
      {
        _x2927 = kk_string_empty(); /*string*/
      }
      _match2928: ;
    }
    kk_string_t _x2932;
    kk_define_string_literal(, _s2933, 3, "exe")
    _x2932 = kk_string_dup(_s2933); /*string*/
    _match_2637 = kk_string_is_eq(_x2927,_x2932,kk_context()); /*bool*/
    if (_match_2637) {
      kk_string_t _x2934;
      kk_std_core_types__optional _match_2638 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
      if (kk_std_core_types__is_Optional(_match_2638)) {
        kk_box_t _box_x2401 = _match_2638._cons.Optional.value;
        kk_string_t _root_1051 = kk_string_unbox(_box_x2401);
        kk_string_dup(_root_1051);
        kk_std_core_types__optional_drop(_match_2638, _ctx);
        _x2934 = _root_1051; /*string*/
        goto _match2935;
      }
      {
        kk_string_t _x020 = p0.root;
        kk_string_dup(_x020);
        _x2934 = _x020; /*string*/
      }
      _match2935: ;
      kk_std_core__list _x2937;
      {
        kk_std_core__list _x3 = p0.parts;
        kk_std_core__list_dup(_x3);
        kk_std_os_path__path_drop(p0, _ctx);
        if (kk_std_core__is_Cons(_x3)) {
          struct kk_std_core_Cons* _con2939 = kk_std_core__as_Cons(_x3);
          kk_box_t _box_x2402 = _con2939->head;
          kk_std_core__list xx1 = _con2939->tail;
          if (kk_likely(kk_std_core__list_is_unique(_x3))) {
            kk_box_drop(_box_x2402, _ctx);
            kk_std_core__list_free(_x3);
          }
          else {
            kk_std_core__list_dup(xx1);
            kk_std_core__list_decref(_x3, _ctx);
          }
          _x2937 = xx1; /*list<string>*/
          goto _match2938;
        }
        {
          _x2937 = kk_std_core__new_Nil(_ctx); /*list<string>*/
        }
        _match2938: ;
      }
      _x2912 = kk_std_os_path__new_Path(_x2934, _x2937, _ctx); /*std/os/path/path*/
    }
    else {
      _x2912 = p0; /*std/os/path/path*/
    }
  }
  return kk_std_os_path__path_box(_x2912, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2212_appdir(kk_string_t _y_2179, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_std_os_path__path _x1_2196;
  kk_box_t _x2895 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2212_appdir_fun2896(_ctx), kk_string_box(_y_2179), _ctx); /*1002*/
  _x1_2196 = kk_std_os_path__path_unbox(_x2895, _ctx); /*std/os/path/path*/
  kk_std_os_path__path p0;
  kk_box_t _x2899 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2212_appdir_fun2900(_ctx), kk_std_os_path__path_box(_x1_2196, _ctx), _ctx); /*1002*/
  p0 = kk_std_os_path__path_unbox(_x2899, _ctx); /*std/os/path/path*/
  kk_box_t _x2910 = kk_std_core_hnd__open_none0(kk_std_os_path__new_mlift2212_appdir_fun2911(p0, _ctx), _ctx); /*1001*/
  return kk_std_os_path__path_unbox(_x2910, _ctx);
}
 
// Return the base directory that contains the currently running application.
// First tries `app-path().nobase`; if that ends in the ``bin`` or ``exe`` directory it
// returns the parent of that directory.


// lift anonymous function
struct kk_std_os_path_appdir_fun2942__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_appdir_fun2942(kk_function_t _fself, kk_box_t _b_2406, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_appdir_fun2942(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_appdir_fun2942, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_appdir_fun2942(kk_function_t _fself, kk_box_t _b_2406, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2943;
  kk_string_t _x2944 = kk_string_unbox(_b_2406); /*string*/
  _x2943 = kk_std_os_path__mlift2212_appdir(_x2944, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2943, _ctx);
}


// lift anonymous function
struct kk_std_os_path_appdir_fun2946__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_appdir_fun2946(kk_function_t _fself, kk_box_t _b_2409, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_appdir_fun2946(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_appdir_fun2946, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_appdir_fun2946(kk_function_t _fself, kk_box_t _b_2409, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2947;
  kk_string_t _x2948 = kk_string_unbox(_b_2409); /*string*/
  _x2947 = kk_std_os_path_path(_x2948, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2947, _ctx);
}


// lift anonymous function
struct kk_std_os_path_appdir_fun2950__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_appdir_fun2950(kk_function_t _fself, kk_box_t _b_2416, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_appdir_fun2950(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_appdir_fun2950, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_appdir_fun2950(kk_function_t _fself, kk_box_t _b_2416, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x2951;
  kk_string_t _x2952;
  kk_std_core_types__optional _match_2634 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_2634)) {
    kk_box_t _box_x2412 = _match_2634._cons.Optional.value;
    kk_string_t _root_105 = kk_string_unbox(_box_x2412);
    kk_string_dup(_root_105);
    kk_std_core_types__optional_drop(_match_2634, _ctx);
    _x2952 = _root_105; /*string*/
    goto _match2953;
  }
  {
    kk_std_os_path__path _match_2635;
    kk_box_t _x2955 = kk_box_dup(_b_2416); /*1001*/
    _match_2635 = kk_std_os_path__path_unbox(_x2955, _ctx); /*std/os/path/path*/
    {
      kk_string_t _x00 = _match_2635.root;
      kk_string_dup(_x00);
      kk_std_os_path__path_drop(_match_2635, _ctx);
      _x2952 = _x00; /*string*/
    }
  }
  _match2953: ;
  kk_std_core__list _x2956;
  kk_std_os_path__path _match_2633 = kk_std_os_path__path_unbox(_b_2416, _ctx); /*std/os/path/path*/;
  {
    kk_std_core__list _x = _match_2633.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(_match_2633, _ctx);
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con2958 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2413 = _con2958->head;
      kk_std_core__list xx = _con2958->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_box_drop(_box_x2413, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x, _ctx);
      }
      _x2956 = xx; /*list<string>*/
      goto _match2957;
    }
    {
      _x2956 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match2957: ;
  }
  _x2951 = kk_std_os_path__new_Path(_x2952, _x2956, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x2951, _ctx);
}


// lift anonymous function
struct kk_std_os_path_appdir_fun2961__t {
  struct kk_function_s _base;
  kk_std_os_path__path p0;
};
static kk_box_t kk_std_os_path_appdir_fun2961(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_appdir_fun2961(kk_std_os_path__path p0, kk_context_t* _ctx) {
  struct kk_std_os_path_appdir_fun2961__t* _self = kk_function_alloc_as(struct kk_std_os_path_appdir_fun2961__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_os_path_appdir_fun2961, kk_context());
  _self->p0 = p0;
  return &_self->_base;
}

static kk_box_t kk_std_os_path_appdir_fun2961(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_os_path_appdir_fun2961__t* _self = kk_function_as(struct kk_std_os_path_appdir_fun2961__t*, _fself);
  kk_std_os_path__path p0 = _self->p0; /* std/os/path/path */
  kk_drop_match(_self, {kk_std_os_path__path_dup(p0);}, {}, _ctx)
  kk_std_os_path__path _x2962;
  bool _match_2629;
  kk_string_t _x2963;
  {
    kk_std_core__list _x11 = p0.parts;
    kk_std_core__list_dup(_x11);
    if (kk_std_core__is_Cons(_x11)) {
      struct kk_std_core_Cons* _con2965 = kk_std_core__as_Cons(_x11);
      kk_box_t _box_x2420 = _con2965->head;
      kk_std_core__list _pat030 = _con2965->tail;
      kk_string_t x0 = kk_string_unbox(_box_x2420);
      if (kk_likely(kk_std_core__list_is_unique(_x11))) {
        kk_std_core__list_drop(_pat030, _ctx);
        kk_std_core__list_free(_x11);
      }
      else {
        kk_string_dup(x0);
        kk_std_core__list_decref(_x11, _ctx);
      }
      _x2963 = x0; /*string*/
      goto _match2964;
    }
    {
      _x2963 = kk_string_empty(); /*string*/
    }
    _match2964: ;
  }
  kk_string_t _x2968;
  kk_define_string_literal(, _s2969, 3, "bin")
  _x2968 = kk_string_dup(_s2969); /*string*/
  _match_2629 = kk_string_is_eq(_x2963,_x2968,kk_context()); /*bool*/
  if (_match_2629) {
    kk_string_t _x2970;
    kk_std_core_types__optional _match_2632 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_2632)) {
      kk_box_t _box_x2421 = _match_2632._cons.Optional.value;
      kk_string_t _root_1050 = kk_string_unbox(_box_x2421);
      kk_string_dup(_root_1050);
      kk_std_core_types__optional_drop(_match_2632, _ctx);
      _x2970 = _root_1050; /*string*/
      goto _match2971;
    }
    {
      kk_string_t _x000 = p0.root;
      kk_string_dup(_x000);
      _x2970 = _x000; /*string*/
    }
    _match2971: ;
    kk_std_core__list _x2973;
    {
      kk_std_core__list _x2 = p0.parts;
      kk_std_core__list_dup(_x2);
      kk_std_os_path__path_drop(p0, _ctx);
      if (kk_std_core__is_Cons(_x2)) {
        struct kk_std_core_Cons* _con2975 = kk_std_core__as_Cons(_x2);
        kk_box_t _box_x2422 = _con2975->head;
        kk_std_core__list xx0 = _con2975->tail;
        if (kk_likely(kk_std_core__list_is_unique(_x2))) {
          kk_box_drop(_box_x2422, _ctx);
          kk_std_core__list_free(_x2);
        }
        else {
          kk_std_core__list_dup(xx0);
          kk_std_core__list_decref(_x2, _ctx);
        }
        _x2973 = xx0; /*list<string>*/
        goto _match2974;
      }
      {
        _x2973 = kk_std_core__new_Nil(_ctx); /*list<string>*/
      }
      _match2974: ;
    }
    _x2962 = kk_std_os_path__new_Path(_x2970, _x2973, _ctx); /*std/os/path/path*/
  }
  else {
    bool _match_2630;
    kk_string_t _x2977;
    {
      kk_std_core__list _x010 = p0.parts;
      kk_std_core__list_dup(_x010);
      if (kk_std_core__is_Cons(_x010)) {
        struct kk_std_core_Cons* _con2979 = kk_std_core__as_Cons(_x010);
        kk_box_t _box_x2423 = _con2979->head;
        kk_std_core__list _pat021 = _con2979->tail;
        kk_string_t x00 = kk_string_unbox(_box_x2423);
        if (kk_likely(kk_std_core__list_is_unique(_x010))) {
          kk_std_core__list_drop(_pat021, _ctx);
          kk_std_core__list_free(_x010);
        }
        else {
          kk_string_dup(x00);
          kk_std_core__list_decref(_x010, _ctx);
        }
        _x2977 = x00; /*string*/
        goto _match2978;
      }
      {
        _x2977 = kk_string_empty(); /*string*/
      }
      _match2978: ;
    }
    kk_string_t _x2982;
    kk_define_string_literal(, _s2983, 3, "exe")
    _x2982 = kk_string_dup(_s2983); /*string*/
    _match_2630 = kk_string_is_eq(_x2977,_x2982,kk_context()); /*bool*/
    if (_match_2630) {
      kk_string_t _x2984;
      kk_std_core_types__optional _match_2631 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
      if (kk_std_core_types__is_Optional(_match_2631)) {
        kk_box_t _box_x2424 = _match_2631._cons.Optional.value;
        kk_string_t _root_1051 = kk_string_unbox(_box_x2424);
        kk_string_dup(_root_1051);
        kk_std_core_types__optional_drop(_match_2631, _ctx);
        _x2984 = _root_1051; /*string*/
        goto _match2985;
      }
      {
        kk_string_t _x020 = p0.root;
        kk_string_dup(_x020);
        _x2984 = _x020; /*string*/
      }
      _match2985: ;
      kk_std_core__list _x2987;
      {
        kk_std_core__list _x3 = p0.parts;
        kk_std_core__list_dup(_x3);
        kk_std_os_path__path_drop(p0, _ctx);
        if (kk_std_core__is_Cons(_x3)) {
          struct kk_std_core_Cons* _con2989 = kk_std_core__as_Cons(_x3);
          kk_box_t _box_x2425 = _con2989->head;
          kk_std_core__list xx1 = _con2989->tail;
          if (kk_likely(kk_std_core__list_is_unique(_x3))) {
            kk_box_drop(_box_x2425, _ctx);
            kk_std_core__list_free(_x3);
          }
          else {
            kk_std_core__list_dup(xx1);
            kk_std_core__list_decref(_x3, _ctx);
          }
          _x2987 = xx1; /*list<string>*/
          goto _match2988;
        }
        {
          _x2987 = kk_std_core__new_Nil(_ctx); /*list<string>*/
        }
        _match2988: ;
      }
      _x2962 = kk_std_os_path__new_Path(_x2984, _x2987, _ctx); /*std/os/path/path*/
    }
    else {
      _x2962 = p0; /*std/os/path/path*/
    }
  }
  return kk_std_os_path__path_box(_x2962, _ctx);
}

kk_std_os_path__path kk_std_os_path_appdir(kk_context_t* _ctx) { /* () -> io path */ 
  kk_string_t x_2242 = kk_std_os_path_xapp_path(_ctx); /*string*/;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2242, _ctx);
    kk_box_t _x2941 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_appdir_fun2942(_ctx), _ctx); /*1002*/
    return kk_std_os_path__path_unbox(_x2941, _ctx);
  }
  {
    kk_std_os_path__path _x1_2196;
    kk_box_t _x2945 = kk_std_core_hnd__open_none1(kk_std_os_path_new_appdir_fun2946(_ctx), kk_string_box(x_2242), _ctx); /*1002*/
    _x1_2196 = kk_std_os_path__path_unbox(_x2945, _ctx); /*std/os/path/path*/
    kk_std_os_path__path p0;
    kk_box_t _x2949 = kk_std_core_hnd__open_none1(kk_std_os_path_new_appdir_fun2950(_ctx), kk_std_os_path__path_box(_x1_2196, _ctx), _ctx); /*1002*/
    p0 = kk_std_os_path__path_unbox(_x2949, _ctx); /*std/os/path/path*/
    kk_box_t _x2960 = kk_std_core_hnd__open_none0(kk_std_os_path_new_appdir_fun2961(p0, _ctx), _ctx); /*1001*/
    return kk_std_os_path__path_unbox(_x2960, _ctx);
  }
}
 
// Change the base name of a path

kk_std_os_path__path kk_std_os_path_change_base(kk_std_os_path__path p, kk_string_t basename0, kk_context_t* _ctx) { /* (p : path, basename : string) -> path */ 
  kk_std_os_path__path q;
  kk_string_t _x2991;
  kk_std_core_types__optional _match_2627 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_2627)) {
    kk_box_t _box_x2429 = _match_2627._cons.Optional.value;
    kk_string_t _root_105 = kk_string_unbox(_box_x2429);
    kk_string_dup(_root_105);
    kk_std_core_types__optional_drop(_match_2627, _ctx);
    _x2991 = _root_105; /*string*/
    goto _match2992;
  }
  {
    kk_string_t _x0 = p.root;
    kk_string_dup(_x0);
    _x2991 = _x0; /*string*/
  }
  _match2992: ;
  kk_std_core__list _x2994;
  {
    kk_std_core__list _x = p.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con2996 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2430 = _con2996->head;
      kk_std_core__list xx = _con2996->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_box_drop(_box_x2430, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x, _ctx);
      }
      _x2994 = xx; /*list<string>*/
      goto _match2995;
    }
    {
      _x2994 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match2995: ;
  }
  q = kk_std_os_path__new_Path(_x2991, _x2994, _ctx); /*std/os/path/path*/
  kk_std_core__list parts0;
  kk_std_core__list _x2998;
  kk_vector_t v_16666;
  kk_string_t _x2999;
  kk_define_string_literal(, _s3000, 1, "/")
  _x2999 = kk_string_dup(_s3000); /*string*/
  v_16666 = kk_string_splitv(basename0,_x2999,kk_context()); /*vector<string>*/
  _x2998 = kk_std_core_vlist(v_16666, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
  kk_std_core__list _x3001;
  {
    kk_std_core__list _x00 = q.parts;
    kk_std_core__list_dup(_x00);
    _x3001 = _x00; /*list<string>*/
  }
  parts0 = kk_std_os_path_push_parts(_x2998, _x3001, _ctx); /*list<string>*/
  kk_string_t _x3002;
  {
    kk_string_t _x1 = q.root;
    kk_string_dup(_x1);
    kk_std_os_path__path_drop(q, _ctx);
    _x3002 = _x1; /*string*/
  }
  return kk_std_os_path__new_Path(_x3002, parts0, _ctx);
}

kk_std_core_types__tuple2_ kk_std_os_path_split_base(kk_string_t basename0, kk_context_t* _ctx) { /* (basename : string) -> (string, string) */ 
  kk_std_core_types__maybe _match_2626;
  kk_string_t _x3003 = kk_string_dup(basename0); /*string*/
  kk_string_t _x3004;
  kk_define_string_literal(, _s3005, 1, ".")
  _x3004 = kk_string_dup(_s3005); /*string*/
  _match_2626 = kk_std_core_find_last(_x3003, _x3004, _ctx); /*maybe<sslice>*/
  if (kk_std_core_types__is_Just(_match_2626)) {
    kk_box_t _box_x2431 = _match_2626._cons.Just.value;
    kk_std_core__sslice slice = kk_std_core__sslice_unbox(_box_x2431, NULL);
    kk_string_drop(basename0, _ctx);
    kk_string_t _b_2436_2432;
    kk_std_core__sslice _x3007;
    {
      kk_string_t s = slice.str;
      kk_ssize_t start0 = slice.start;
      kk_string_dup(s);
      kk_ssize_t _x3008 = ((kk_ssize_t)0); /*ssize_t*/
      _x3007 = kk_std_core__new_Sslice(s, _x3008, start0, _ctx); /*sslice*/
    }
    _b_2436_2432 = kk_std_core_string_3(_x3007, _ctx); /*string*/
    kk_string_t _b_2437_2433;
    kk_std_core__sslice _x3009 = kk_std_core_after(slice, _ctx); /*sslice*/
    _b_2437_2433 = kk_std_core_string_3(_x3009, _ctx); /*string*/
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(_b_2436_2432), kk_string_box(_b_2437_2433), _ctx);
  }
  {
    kk_box_t _x3010;
    kk_string_t _x3011 = kk_string_empty(); /*string*/
    _x3010 = kk_string_box(_x3011); /*1005*/
    return kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(basename0), _x3010, _ctx);
  }
}
 
// Change the extension of a path.
// Only adds a dot if the extname does not already start with a dot.

kk_std_os_path__path kk_std_os_path_change_ext(kk_std_os_path__path p, kk_string_t extname0, kk_context_t* _ctx) { /* (p : path, extname : string) -> path */ 
  kk_std_core_types__tuple2_ _match_2625;
  kk_string_t _x3013;
  {
    kk_std_core__list _x = p.parts;
    kk_std_core__list_dup(_x);
    if (kk_std_core__is_Cons(_x)) {
      struct kk_std_core_Cons* _con3015 = kk_std_core__as_Cons(_x);
      kk_box_t _box_x2440 = _con3015->head;
      kk_std_core__list _pat0 = _con3015->tail;
      kk_string_t x = kk_string_unbox(_box_x2440);
      if (kk_likely(kk_std_core__list_is_unique(_x))) {
        kk_std_core__list_drop(_pat0, _ctx);
        kk_std_core__list_free(_x);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x, _ctx);
      }
      _x3013 = x; /*string*/
      goto _match3014;
    }
    {
      _x3013 = kk_string_empty(); /*string*/
    }
    _match3014: ;
  }
  _match_2625 = kk_std_os_path_split_base(_x3013, _ctx); /*(string, string)*/
  {
    kk_box_t _box_x2441 = _match_2625.fst;
    kk_box_t _box_x2442 = _match_2625.snd;
    kk_string_t stemname0 = kk_string_unbox(_box_x2441);
    kk_string_dup(stemname0);
    kk_std_core_types__tuple2__drop(_match_2625, _ctx);
    kk_std_core_types__maybe m_2102;
    kk_string_t _x3020 = kk_string_dup(extname0); /*string*/
    kk_string_t _x3021;
    kk_define_string_literal(, _s3022, 1, ".")
    _x3021 = kk_string_dup(_s3022); /*string*/
    m_2102 = kk_std_core_starts_with(_x3020, _x3021, _ctx); /*maybe<sslice>*/
    kk_string_t newext;
    if (kk_std_core_types__is_Nothing(m_2102)) {
      kk_string_t _x3023;
      kk_define_string_literal(, _s3024, 1, ".")
      _x3023 = kk_string_dup(_s3024); /*string*/
      newext = kk_std_core__lp__plus__plus__1_rp_(_x3023, extname0, _ctx); /*string*/
    }
    else {
      kk_std_core_types__maybe_drop(m_2102, _ctx);
      newext = extname0; /*string*/
    }
    kk_string_t s_2104 = kk_std_core__lp__plus__plus__1_rp_(stemname0, newext, _ctx); /*string*/;
    kk_std_core__list parts1;
    kk_std_core__list _x3025;
    kk_vector_t v_16666;
    kk_string_t _x3026;
    kk_define_string_literal(, _s3027, 1, "/")
    _x3026 = kk_string_dup(_s3027); /*string*/
    v_16666 = kk_string_splitv(s_2104,_x3026,kk_context()); /*vector<string>*/
    _x3025 = kk_std_core_vlist(v_16666, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
    kk_std_core__list _x3028;
    {
      kk_std_core__list _x0 = p.parts;
      kk_std_core__list_dup(_x0);
      if (kk_std_core__is_Cons(_x0)) {
        struct kk_std_core_Cons* _con3030 = kk_std_core__as_Cons(_x0);
        kk_box_t _box_x2443 = _con3030->head;
        kk_std_core__list xx = _con3030->tail;
        if (kk_likely(kk_std_core__list_is_unique(_x0))) {
          kk_box_drop(_box_x2443, _ctx);
          kk_std_core__list_free(_x0);
        }
        else {
          kk_std_core__list_dup(xx);
          kk_std_core__list_decref(_x0, _ctx);
        }
        _x3028 = xx; /*list<string>*/
        goto _match3029;
      }
      {
        _x3028 = kk_std_core__new_Nil(_ctx); /*list<string>*/
      }
      _match3029: ;
    }
    parts1 = kk_std_os_path_push_parts(_x3025, _x3028, _ctx); /*list<string>*/
    kk_string_t _x3032;
    {
      kk_string_t _x00 = p.root;
      kk_string_dup(_x00);
      kk_std_os_path__path_drop(p, _ctx);
      _x3032 = _x00; /*string*/
    }
    return kk_std_os_path__new_Path(_x3032, parts1, _ctx);
  }
}
 
// Return the extension of path (without the preceding dot (`'.'`))
// `"/foo/bar.svg.txt".path.extname === "txt"`

kk_string_t kk_std_os_path_extname(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  kk_std_core_types__tuple2_ _this_2109;
  kk_string_t _x3033;
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x0)) {
      struct kk_std_core_Cons* _con3035 = kk_std_core__as_Cons(_x0);
      kk_box_t _box_x2444 = _con3035->head;
      kk_std_core__list _pat00 = _con3035->tail;
      kk_string_t x = kk_string_unbox(_box_x2444);
      if (kk_likely(kk_std_core__list_is_unique(_x0))) {
        kk_std_core__list_drop(_pat00, _ctx);
        kk_std_core__list_free(_x0);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x0, _ctx);
      }
      _x3033 = x; /*string*/
      goto _match3034;
    }
    {
      _x3033 = kk_string_empty(); /*string*/
    }
    _match3034: ;
  }
  _this_2109 = kk_std_os_path_split_base(_x3033, _ctx); /*(string, string)*/
  {
    kk_box_t _box_x2445 = _this_2109.fst;
    kk_box_t _box_x2446 = _this_2109.snd;
    kk_string_t _x = kk_string_unbox(_box_x2446);
    kk_string_dup(_x);
    kk_std_core_types__tuple2__drop(_this_2109, _ctx);
    return _x;
  }
}
 
// Change the stem name of a path

kk_std_os_path__path kk_std_os_path_change_stem(kk_std_os_path__path p, kk_string_t stemname0, kk_context_t* _ctx) { /* (p : path, stemname : string) -> path */ 
  kk_std_core_types__tuple2_ _this_2109;
  kk_string_t _x3040;
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    if (kk_std_core__is_Cons(_x0)) {
      struct kk_std_core_Cons* _con3042 = kk_std_core__as_Cons(_x0);
      kk_box_t _box_x2447 = _con3042->head;
      kk_std_core__list _pat00 = _con3042->tail;
      kk_string_t x = kk_string_unbox(_box_x2447);
      if (kk_likely(kk_std_core__list_is_unique(_x0))) {
        kk_std_core__list_drop(_pat00, _ctx);
        kk_std_core__list_free(_x0);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x0, _ctx);
      }
      _x3040 = x; /*string*/
      goto _match3041;
    }
    {
      _x3040 = kk_string_empty(); /*string*/
    }
    _match3041: ;
  }
  _this_2109 = kk_std_os_path_split_base(_x3040, _ctx); /*(string, string)*/
  kk_string_t basename0_2150;
  kk_string_t _x3045;
  bool _match_2624;
  kk_string_t _x3046;
  {
    kk_box_t _box_x2448 = _this_2109.fst;
    kk_box_t _box_x2449 = _this_2109.snd;
    kk_string_t _x = kk_string_unbox(_box_x2449);
    kk_string_dup(_x);
    _x3046 = _x; /*string*/
  }
  kk_string_t _x3049 = kk_string_empty(); /*string*/
  _match_2624 = kk_string_is_eq(_x3046,_x3049,kk_context()); /*bool*/
  if (_match_2624) {
    kk_std_core_types__tuple2__drop(_this_2109, _ctx);
    _x3045 = kk_string_empty(); /*string*/
  }
  else {
    kk_string_t _x3052;
    kk_define_string_literal(, _s3053, 1, ".")
    _x3052 = kk_string_dup(_s3053); /*string*/
    kk_string_t _x3054;
    {
      kk_box_t _box_x2450 = _this_2109.fst;
      kk_box_t _box_x2451 = _this_2109.snd;
      kk_string_t _x1 = kk_string_unbox(_box_x2451);
      kk_string_dup(_x1);
      kk_std_core_types__tuple2__drop(_this_2109, _ctx);
      _x3054 = _x1; /*string*/
    }
    _x3045 = kk_std_core__lp__plus__plus__1_rp_(_x3052, _x3054, _ctx); /*string*/
  }
  basename0_2150 = kk_std_core__lp__plus__plus__1_rp_(stemname0, _x3045, _ctx); /*string*/
  kk_std_os_path__path q;
  kk_string_t _x3057;
  kk_std_core_types__optional _match_2623 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
  if (kk_std_core_types__is_Optional(_match_2623)) {
    kk_box_t _box_x2452 = _match_2623._cons.Optional.value;
    kk_string_t _root_105 = kk_string_unbox(_box_x2452);
    kk_string_dup(_root_105);
    kk_std_core_types__optional_drop(_match_2623, _ctx);
    _x3057 = _root_105; /*string*/
    goto _match3058;
  }
  {
    kk_string_t _x00 = p.root;
    kk_string_dup(_x00);
    _x3057 = _x00; /*string*/
  }
  _match3058: ;
  kk_std_core__list _x3060;
  {
    kk_std_core__list _x10 = p.parts;
    kk_std_core__list_dup(_x10);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x10)) {
      struct kk_std_core_Cons* _con3062 = kk_std_core__as_Cons(_x10);
      kk_box_t _box_x2453 = _con3062->head;
      kk_std_core__list xx = _con3062->tail;
      if (kk_likely(kk_std_core__list_is_unique(_x10))) {
        kk_box_drop(_box_x2453, _ctx);
        kk_std_core__list_free(_x10);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_x10, _ctx);
      }
      _x3060 = xx; /*list<string>*/
      goto _match3061;
    }
    {
      _x3060 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match3061: ;
  }
  q = kk_std_os_path__new_Path(_x3057, _x3060, _ctx); /*std/os/path/path*/
  kk_std_core__list parts0;
  kk_std_core__list _x3064;
  kk_vector_t v_16666;
  kk_string_t _x3065;
  kk_define_string_literal(, _s3066, 1, "/")
  _x3065 = kk_string_dup(_s3066); /*string*/
  v_16666 = kk_string_splitv(basename0_2150,_x3065,kk_context()); /*vector<string>*/
  _x3064 = kk_std_core_vlist(v_16666, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
  kk_std_core__list _x3067;
  {
    kk_std_core__list _x01 = q.parts;
    kk_std_core__list_dup(_x01);
    _x3067 = _x01; /*list<string>*/
  }
  parts0 = kk_std_os_path_push_parts(_x3064, _x3067, _ctx); /*list<string>*/
  kk_string_t _x3068;
  {
    kk_string_t _x2 = q.root;
    kk_string_dup(_x2);
    kk_std_os_path__path_drop(q, _ctx);
    _x3068 = _x2; /*string*/
  }
  return kk_std_os_path__new_Path(_x3068, parts0, _ctx);
}
 
// Combine multiple paths using `(/)`.


// lift anonymous function
struct kk_std_os_path_combine_fun3079__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_combine_fun3079(kk_function_t _fself, kk_box_t _b_2460, kk_box_t _b_2461, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_combine_fun3079(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_combine_fun3079, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_combine_fun3079(kk_function_t _fself, kk_box_t _b_2460, kk_box_t _b_2461, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3080;
  kk_std_os_path__path _x3081 = kk_std_os_path__path_unbox(_b_2460, _ctx); /*std/os/path/path*/
  kk_std_os_path__path _x3082 = kk_std_os_path__path_unbox(_b_2461, _ctx); /*std/os/path/path*/
  _x3080 = kk_std_os_path__lp__fs__rp_(_x3081, _x3082, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3080, _ctx);
}

kk_std_os_path__path kk_std_os_path_combine(kk_std_core__list ps, kk_context_t* _ctx) { /* (ps : list<path>) -> path */ 
  if (kk_std_core__is_Nil(ps)) {
    kk_string_t _x3069;
    kk_std_core_types__optional _match_2622 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_2622)) {
      kk_box_t _box_x2454 = _match_2622._cons.Optional.value;
      kk_string_t _root_871 = kk_string_unbox(_box_x2454);
      kk_string_dup(_root_871);
      kk_std_core_types__optional_drop(_match_2622, _ctx);
      _x3069 = _root_871; /*string*/
      goto _match3070;
    }
    {
      _x3069 = kk_string_empty(); /*string*/
    }
    _match3070: ;
    kk_std_core__list _x3073;
    kk_std_core_types__optional _match_2621 = kk_std_core_types__new_None(_ctx); /*forall<a> optional<a>*/;
    if (kk_std_core_types__is_Optional(_match_2621)) {
      kk_box_t _box_x2455 = _match_2621._cons.Optional.value;
      kk_std_core__list _parts_876 = kk_std_core__list_unbox(_box_x2455, NULL);
      kk_std_core__list_dup(_parts_876);
      kk_std_core_types__optional_drop(_match_2621, _ctx);
      _x3073 = _parts_876; /*list<string>*/
      goto _match3074;
    }
    {
      _x3073 = kk_std_core__new_Nil(_ctx); /*list<string>*/
    }
    _match3074: ;
    return kk_std_os_path__new_Path(_x3069, _x3073, _ctx);
  }
  {
    struct kk_std_core_Cons* _con3076 = kk_std_core__as_Cons(ps);
    kk_box_t _box_x2456 = _con3076->head;
    kk_std_core__list pp = _con3076->tail;
    kk_std_os_path__path p = kk_std_os_path__path_unbox(_box_x2456, NULL);
    if (kk_likely(kk_std_core__list_is_unique(ps))) {
      kk_std_os_path__path_dup(p);
      kk_box_drop(_box_x2456, _ctx);
      kk_std_core__list_free(ps);
    }
    else {
      kk_std_os_path__path_dup(p);
      kk_std_core__list_dup(pp);
      kk_std_core__list_decref(ps, _ctx);
    }
    kk_box_t _x3078 = kk_std_core_foldl(pp, kk_std_os_path__path_box(p, _ctx), kk_std_os_path_new_combine_fun3079(_ctx), _ctx); /*1002*/
    return kk_std_os_path__path_unbox(_x3078, _ctx);
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2213_realpath_fun3084__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2213_realpath_fun3084_1(kk_function_t _fself, kk_box_t _b_2467, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2213_realpath_fun3084_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2213_realpath_fun3084_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2213_realpath_fun3084_1(kk_function_t _fself, kk_box_t _b_2467, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3085;
  kk_string_t _x3086 = kk_string_unbox(_b_2467); /*string*/
  _x3085 = kk_std_os_path_path(_x3086, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3085, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2213_realpath_1(kk_string_t _y_2180, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_box_t _x3083 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2213_realpath_fun3084_1(_ctx), kk_string_box(_y_2180), _ctx); /*1002*/
  return kk_std_os_path__path_unbox(_x3083, _ctx);
}
 
// Convert a path to the absolute path on the file system.
// The overload on a plain string is necessary as it allows
// for unnormalized paths with `".."` parts. For example
// `"/foo/symlink/../test.txt"` may resolve to `"/bar/test.txt"` if
// ``symlink`` is a symbolic link to a sub directory of `"/bar"`.


// lift anonymous function
struct kk_std_os_path_realpath_fun3088__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_realpath_fun3088_1(kk_function_t _fself, kk_box_t _b_2471, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_realpath_fun3088_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_realpath_fun3088_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_realpath_fun3088_1(kk_function_t _fself, kk_box_t _b_2471, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3089;
  kk_string_t _x3090 = kk_string_unbox(_b_2471); /*string*/
  _x3089 = kk_std_os_path__mlift2213_realpath_1(_x3090, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3089, _ctx);
}


// lift anonymous function
struct kk_std_os_path_realpath_fun3091__t_1 {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_realpath_fun3091_1(kk_function_t _fself, kk_box_t _b_2474, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_realpath_fun3091_1(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_realpath_fun3091_1, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_realpath_fun3091_1(kk_function_t _fself, kk_box_t _b_2474, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3092;
  kk_string_t _x3093 = kk_string_unbox(_b_2474); /*string*/
  _x3092 = kk_std_os_path_path(_x3093, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3092, _ctx);
}

kk_std_os_path__path kk_std_os_path_realpath_1(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> io path */ 
  kk_string_t x_2245 = kk_std_os_path_xrealpath(s, _ctx); /*string*/;
  kk_box_t _x3087;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2245, _ctx);
    _x3087 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_realpath_fun3088_1(_ctx), _ctx); /*1002*/
  }
  else {
    _x3087 = kk_std_core_hnd__open_none1(kk_std_os_path_new_realpath_fun3091_1(_ctx), kk_string_box(x_2245), _ctx); /*1002*/
  }
  return kk_std_os_path__path_unbox(_x3087, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2214_realpath_fun3095__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2214_realpath_fun3095(kk_function_t _fself, kk_box_t _b_2480, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2214_realpath_fun3095(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2214_realpath_fun3095, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2214_realpath_fun3095(kk_function_t _fself, kk_box_t _b_2480, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3096;
  kk_string_t _x3097 = kk_string_unbox(_b_2480); /*string*/
  _x3096 = kk_std_os_path_path(_x3097, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3096, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2214_realpath(kk_string_t _y_2181, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_box_t _x3094 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2214_realpath_fun3095(_ctx), kk_string_box(_y_2181), _ctx); /*1002*/
  return kk_std_os_path__path_unbox(_x3094, _ctx);
}
 
// Convert a path to the absolute path on the file system.
// The path is not required to exist on disk. However, if it
// exists any permissions and symbolic links are resolved fully.
// `".".realpath` (to get the current working directory)
// `"/foo".realpath` (to resolve the full root, like `"c:/foo"` on windows)


// lift anonymous function
struct kk_std_os_path_realpath_fun3099__t {
  struct kk_function_s _base;
  kk_std_os_path__path p;
};
static kk_box_t kk_std_os_path_realpath_fun3099(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_realpath_fun3099(kk_std_os_path__path p, kk_context_t* _ctx) {
  struct kk_std_os_path_realpath_fun3099__t* _self = kk_function_alloc_as(struct kk_std_os_path_realpath_fun3099__t, 3, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_os_path_realpath_fun3099, kk_context());
  _self->p = p;
  return &_self->_base;
}

static kk_box_t kk_std_os_path_realpath_fun3099(kk_function_t _fself, kk_context_t* _ctx) {
  struct kk_std_os_path_realpath_fun3099__t* _self = kk_function_as(struct kk_std_os_path_realpath_fun3099__t*, _fself);
  kk_std_os_path__path p = _self->p; /* std/os/path/path */
  kk_drop_match(_self, {kk_std_os_path__path_dup(p);}, {}, _ctx)
  kk_string_t _x3100 = kk_std_os_path_string(p, _ctx); /*string*/
  return kk_string_box(_x3100);
}


// lift anonymous function
struct kk_std_os_path_realpath_fun3102__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_realpath_fun3102(kk_function_t _fself, kk_box_t _b_2486, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_realpath_fun3102(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_realpath_fun3102, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_realpath_fun3102(kk_function_t _fself, kk_box_t _b_2486, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3103;
  kk_string_t _x3104 = kk_string_unbox(_b_2486); /*string*/
  _x3103 = kk_std_os_path__mlift2214_realpath(_x3104, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3103, _ctx);
}


// lift anonymous function
struct kk_std_os_path_realpath_fun3105__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_realpath_fun3105(kk_function_t _fself, kk_box_t _b_2489, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_realpath_fun3105(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_realpath_fun3105, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_realpath_fun3105(kk_function_t _fself, kk_box_t _b_2489, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3106;
  kk_string_t _x3107 = kk_string_unbox(_b_2489); /*string*/
  _x3106 = kk_std_os_path_path(_x3107, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3106, _ctx);
}

kk_std_os_path__path kk_std_os_path_realpath(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> io path */ 
  kk_string_t s_2117;
  kk_box_t _x3098 = kk_std_core_hnd__open_none0(kk_std_os_path_new_realpath_fun3099(p, _ctx), _ctx); /*1001*/
  s_2117 = kk_string_unbox(_x3098); /*string*/
  kk_string_t x_2248 = kk_std_os_path_xrealpath(s_2117, _ctx); /*string*/;
  kk_box_t _x3101;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2248, _ctx);
    _x3101 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_realpath_fun3102(_ctx), _ctx); /*1002*/
  }
  else {
    _x3101 = kk_std_core_hnd__open_none1(kk_std_os_path_new_realpath_fun3105(_ctx), kk_string_box(x_2248), _ctx); /*1002*/
  }
  return kk_std_os_path__path_unbox(_x3101, _ctx);
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2215_cwd_fun3109__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2215_cwd_fun3109(kk_function_t _fself, kk_box_t _b_2495, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2215_cwd_fun3109(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2215_cwd_fun3109, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2215_cwd_fun3109(kk_function_t _fself, kk_box_t _b_2495, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3110;
  kk_string_t _x3111 = kk_string_unbox(_b_2495); /*string*/
  _x3110 = kk_std_os_path_path(_x3111, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3110, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2215_cwd(kk_string_t _y_2182, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_box_t _x3108 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2215_cwd_fun3109(_ctx), kk_string_box(_y_2182), _ctx); /*1002*/
  return kk_std_os_path__path_unbox(_x3108, _ctx);
}
 
// Returns the current working directory.
// Equal to `".".realpath`.


// lift anonymous function
struct kk_std_os_path_cwd_fun3115__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_cwd_fun3115(kk_function_t _fself, kk_box_t _b_2499, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_cwd_fun3115(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_cwd_fun3115, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_cwd_fun3115(kk_function_t _fself, kk_box_t _b_2499, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3116;
  kk_string_t _x3117 = kk_string_unbox(_b_2499); /*string*/
  _x3116 = kk_std_os_path__mlift2215_cwd(_x3117, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3116, _ctx);
}


// lift anonymous function
struct kk_std_os_path_cwd_fun3118__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_cwd_fun3118(kk_function_t _fself, kk_box_t _b_2502, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_cwd_fun3118(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_cwd_fun3118, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_cwd_fun3118(kk_function_t _fself, kk_box_t _b_2502, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3119;
  kk_string_t _x3120 = kk_string_unbox(_b_2502); /*string*/
  _x3119 = kk_std_os_path_path(_x3120, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3119, _ctx);
}

kk_std_os_path__path kk_std_os_path_cwd(kk_context_t* _ctx) { /* () -> io path */ 
  kk_string_t x_2251;
  kk_string_t _x3112;
  kk_define_string_literal(, _s3113, 1, ".")
  _x3112 = kk_string_dup(_s3113); /*string*/
  x_2251 = kk_std_os_path_xrealpath(_x3112, _ctx); /*string*/
  kk_box_t _x3114;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2251, _ctx);
    _x3114 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_cwd_fun3115(_ctx), _ctx); /*1002*/
  }
  else {
    _x3114 = kk_std_core_hnd__open_none1(kk_std_os_path_new_cwd_fun3118(_ctx), kk_string_box(x_2251), _ctx); /*1002*/
  }
  return kk_std_os_path__path_unbox(_x3114, _ctx);
}
 
// If a path has no extension, set it to the provided one.

kk_std_os_path__path kk_std_os_path_default_ext(kk_std_os_path__path p, kk_string_t newext, kk_context_t* _ctx) { /* (p : path, newext : string) -> path */ 
  kk_std_core_types__tuple2_ _this_2109;
  kk_string_t _x3121;
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    if (kk_std_core__is_Cons(_x0)) {
      struct kk_std_core_Cons* _con3123 = kk_std_core__as_Cons(_x0);
      kk_box_t _box_x2506 = _con3123->head;
      kk_std_core__list _pat00 = _con3123->tail;
      kk_string_t x = kk_string_unbox(_box_x2506);
      if (kk_likely(kk_std_core__list_is_unique(_x0))) {
        kk_std_core__list_drop(_pat00, _ctx);
        kk_std_core__list_free(_x0);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x0, _ctx);
      }
      _x3121 = x; /*string*/
      goto _match3122;
    }
    {
      _x3121 = kk_string_empty(); /*string*/
    }
    _match3122: ;
  }
  _this_2109 = kk_std_os_path_split_base(_x3121, _ctx); /*(string, string)*/
  bool _match_2617;
  kk_string_t _x3126;
  {
    kk_box_t _box_x2507 = _this_2109.fst;
    kk_box_t _box_x2508 = _this_2109.snd;
    kk_string_t _x = kk_string_unbox(_box_x2508);
    kk_string_dup(_x);
    kk_std_core_types__tuple2__drop(_this_2109, _ctx);
    _x3126 = _x; /*string*/
  }
  kk_string_t _x3129 = kk_string_empty(); /*string*/
  _match_2617 = kk_string_is_eq(_x3126,_x3129,kk_context()); /*bool*/
  if (_match_2617) {
    return kk_std_os_path_change_ext(p, newext, _ctx);
  }
  {
    kk_string_drop(newext, _ctx);
    return p;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2216_homedir_fun3132__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2216_homedir_fun3132(kk_function_t _fself, kk_box_t _b_2511, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2216_homedir_fun3132(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2216_homedir_fun3132, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2216_homedir_fun3132(kk_function_t _fself, kk_box_t _b_2511, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3133;
  kk_string_t _x3134 = kk_string_unbox(_b_2511); /*string*/
  _x3133 = kk_std_os_path_path(_x3134, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3133, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2216_homedir(kk_string_t _y_2183, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_box_t _x3131 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2216_homedir_fun3132(_ctx), kk_string_box(_y_2183), _ctx); /*1002*/
  return kk_std_os_path__path_unbox(_x3131, _ctx);
}
 
// Return the home directory of the current user.


// lift anonymous function
struct kk_std_os_path_homedir_fun3136__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_homedir_fun3136(kk_function_t _fself, kk_box_t _b_2515, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_homedir_fun3136(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_homedir_fun3136, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_homedir_fun3136(kk_function_t _fself, kk_box_t _b_2515, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3137;
  kk_string_t _x3138 = kk_string_unbox(_b_2515); /*string*/
  _x3137 = kk_std_os_path__mlift2216_homedir(_x3138, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3137, _ctx);
}


// lift anonymous function
struct kk_std_os_path_homedir_fun3139__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_homedir_fun3139(kk_function_t _fself, kk_box_t _b_2518, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_homedir_fun3139(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_homedir_fun3139, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_homedir_fun3139(kk_function_t _fself, kk_box_t _b_2518, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3140;
  kk_string_t _x3141 = kk_string_unbox(_b_2518); /*string*/
  _x3140 = kk_std_os_path_path(_x3141, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3140, _ctx);
}

kk_std_os_path__path kk_std_os_path_homedir(kk_context_t* _ctx) { /* () -> io path */ 
  kk_string_t x_2254 = kk_std_os_path_xhomedir(_ctx); /*string*/;
  kk_box_t _x3135;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2254, _ctx);
    _x3135 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_homedir_fun3136(_ctx), _ctx); /*1002*/
  }
  else {
    _x3135 = kk_std_core_hnd__open_none1(kk_std_os_path_new_homedir_fun3139(_ctx), kk_string_box(x_2254), _ctx); /*1002*/
  }
  return kk_std_os_path__path_unbox(_x3135, _ctx);
}


// lift anonymous function
struct kk_std_os_path__ctail_paths_collect_fun3163__t {
  struct kk_function_s _base;
};
static kk_std_core_types__maybe kk_std_os_path__ctail_paths_collect_fun3163(kk_function_t _fself, kk_char_t _b_2524, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_ctail_paths_collect_fun3163(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__ctail_paths_collect_fun3163, _ctx)
  return kk_function_dup(_fself);
}

static kk_std_core_types__maybe kk_std_os_path__ctail_paths_collect_fun3163(kk_function_t _fself, kk_char_t _b_2524, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  return kk_std_core_types__new_Just(kk_char_box(_b_2524, _ctx), _ctx);
}

kk_std_core__list kk_std_os_path__ctail_paths_collect(kk_std_core__list ps, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* (ps : list<string>, ctail<list<path>>) -> list<path> */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(ps)) {
    struct kk_std_core_Cons* _con3150 = kk_std_core__as_Cons(ps);
    kk_box_t _box_x2522 = _con3150->head;
    kk_std_core__list _pat0 = _con3150->tail;
    if (kk_std_core__is_Cons(_pat0)) {
      struct kk_std_core_Cons* _con3152 = kk_std_core__as_Cons(_pat0);
      kk_string_t root0 = kk_string_unbox(_box_x2522);
      kk_box_t _box_x2523 = _con3152->head;
      kk_std_core__list rest = _con3152->tail;
      kk_string_t part = kk_string_unbox(_box_x2523);
      bool _match_2614;
      kk_integer_t _x3154;
      kk_string_t _x3155 = kk_string_dup(root0); /*string*/
      _x3154 = kk_std_core_count_1(_x3155, _ctx); /*int*/
      _match_2614 = kk_integer_eq(_x3154,(kk_integer_from_small(1)),kk_context()); /*bool*/
      bool _x3156;
      if (_match_2614) {
        bool _match_2615;
        kk_char_t _x3157;
        kk_std_core_types__maybe m_2126;
        kk_std_core__sslice _x3158;
        kk_string_t _x3159 = kk_string_dup(root0); /*string*/
        kk_ssize_t _x3160 = ((kk_ssize_t)0); /*ssize_t*/
        kk_ssize_t _x3161;
        kk_string_t _x3162 = kk_string_dup(root0); /*string*/
        _x3161 = kk_string_len(_x3162,kk_context()); /*ssize_t*/
        _x3158 = kk_std_core__new_Sslice(_x3159, _x3160, _x3161, _ctx); /*sslice*/
        m_2126 = kk_std_core_foreach_while_1(_x3158, kk_std_os_path__new_ctail_paths_collect_fun3163(_ctx), _ctx); /*maybe<char>*/
        if (kk_std_core_types__is_Nothing(m_2126)) {
          _x3157 = ' '; /*char*/
        }
        else {
          kk_box_t _box_x2525 = m_2126._cons.Just.value;
          kk_char_t x = kk_char_unbox(_box_x2525, NULL);
          _x3157 = x; /*char*/
        }
        _match_2615 = kk_std_core_is_alpha(_x3157, _ctx); /*bool*/
        if (_match_2615) {
          bool b_2129;
          kk_string_t _x3165 = kk_string_dup(part); /*string*/
          kk_string_t _x3166 = kk_string_empty(); /*string*/
          b_2129 = kk_string_is_eq(_x3165,_x3166,kk_context()); /*bool*/
          if (b_2129) {
            _x3156 = false; /*bool*/
          }
          else {
            kk_string_t _x3168;
            kk_define_string_literal(, _s3169, 2, "/\\")
            _x3168 = kk_string_dup(_s3169); /*string*/
            kk_string_t _x3170;
            kk_string_t _x3171 = kk_string_dup(part); /*string*/
            _x3170 = kk_std_core_head_3(_x3171, _ctx); /*string*/
            _x3156 = kk_string_contains(_x3168,_x3170,kk_context()); /*bool*/
          }
        }
        else {
          _x3156 = false; /*bool*/
        }
      }
      else {
        _x3156 = false; /*bool*/
      }
      if (_x3156) {
        kk_reuse_t _ru_2610 = kk_reuse_null; /*reuse*/;
        if (kk_likely(kk_std_core__list_is_unique(ps))) {
          if (kk_likely(kk_std_core__list_is_unique(_pat0))) {
            _ru_2610 = (kk_std_core__list_reuse(_pat0));
          }
          else {
            kk_string_dup(part);
            kk_std_core__list_dup(rest);
            kk_std_core__list_decref(_pat0, _ctx);
            _ru_2610 = kk_reuse_null;
          }
          kk_std_core__list_free(ps);
        }
        else {
          kk_string_dup(part);
          kk_std_core__list_dup(rest);
          kk_string_dup(root0);
          _ru_2610 = kk_reuse_null;
          kk_std_core__list_decref(ps, _ctx);
        }
        kk_std_os_path__path _ctail_2157;
        kk_string_t _x3172;
        kk_string_t _x3173;
        kk_string_t _x3174;
        kk_define_string_literal(, _s3175, 1, ":")
        _x3174 = kk_string_dup(_s3175); /*string*/
        _x3173 = kk_std_core__lp__plus__plus__1_rp_(_x3174, part, _ctx); /*string*/
        _x3172 = kk_std_core__lp__plus__plus__1_rp_(root0, _x3173, _ctx); /*string*/
        _ctail_2157 = kk_std_os_path_path(_x3172, _ctx); /*std/os/path/path*/
        kk_std_core__list _ctail_2158 = kk_std_core__list_hole(); /*list<std/os/path/path>*/;
        kk_std_core__list _ctail_2159 = kk_std_core__new_Cons(_ru_2610, kk_std_os_path__path_box(_ctail_2157, _ctx), _ctail_2158, _ctx); /*list<std/os/path/path>*/;
        { // tailcall
          kk_std_core_types__ctail _x3176;
          kk_box_t* _b_2551_2535 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_2159)->tail)); /*cfield<list<std/os/path/path>>*/;
          _x3176 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_2159, _ctx)),_b_2551_2535); /*ctail<0>*/
          ps = rest;
          _acc = _x3176;
          goto kk__tailcall;
        }
      }
    }
  }
  if (kk_std_core__is_Cons(ps)) {
    struct kk_std_core_Cons* _con3177 = kk_std_core__as_Cons(ps);
    kk_box_t _box_x2536 = _con3177->head;
    kk_std_core__list rest0 = _con3177->tail;
    kk_string_t part0 = kk_string_unbox(_box_x2536);
    kk_reuse_t _ru_2611 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(ps))) {
      _ru_2611 = (kk_std_core__list_reuse(ps));
    }
    else {
      kk_string_dup(part0);
      kk_std_core__list_dup(rest0);
      kk_std_core__list_decref(ps, _ctx);
      _ru_2611 = kk_reuse_null;
    }
    kk_std_os_path__path _ctail_2160 = kk_std_os_path_path(part0, _ctx); /*std/os/path/path*/;
    kk_std_core__list _ctail_2161 = kk_std_core__list_hole(); /*list<std/os/path/path>*/;
    kk_std_core__list _ctail_2162 = kk_std_core__new_Cons(_ru_2611, kk_std_os_path__path_box(_ctail_2160, _ctx), _ctail_2161, _ctx); /*list<std/os/path/path>*/;
    { // tailcall
      kk_std_core_types__ctail _x3179;
      kk_box_t* _b_2557_2546 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_2162)->tail)); /*cfield<list<std/os/path/path>>*/;
      _x3179 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_2162, _ctx)),_b_2557_2546); /*ctail<0>*/
      ps = rest0;
      _acc = _x3179;
      goto kk__tailcall;
    }
  }
  {
    kk_box_t _x3180 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x3180, _ctx);
  }
}

kk_std_core__list kk_std_os_path_paths_collect(kk_std_core__list ps0, kk_context_t* _ctx) { /* (ps : list<string>) -> list<path> */ 
  kk_std_core_types__ctail _x3181 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_os_path__ctail_paths_collect(ps0, _x3181, _ctx);
}
 
// Return the stem name of path.
// `"/foo/bar.svg.txt".path.extname === "foo.svg"`

kk_string_t kk_std_os_path_stemname(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  kk_std_core_types__tuple2_ _this_2133;
  kk_string_t _x3190;
  {
    kk_std_core__list _x0 = p.parts;
    kk_std_core__list_dup(_x0);
    kk_std_os_path__path_drop(p, _ctx);
    if (kk_std_core__is_Cons(_x0)) {
      struct kk_std_core_Cons* _con3192 = kk_std_core__as_Cons(_x0);
      kk_box_t _box_x2563 = _con3192->head;
      kk_std_core__list _pat00 = _con3192->tail;
      kk_string_t x = kk_string_unbox(_box_x2563);
      if (kk_likely(kk_std_core__list_is_unique(_x0))) {
        kk_std_core__list_drop(_pat00, _ctx);
        kk_std_core__list_free(_x0);
      }
      else {
        kk_string_dup(x);
        kk_std_core__list_decref(_x0, _ctx);
      }
      _x3190 = x; /*string*/
      goto _match3191;
    }
    {
      _x3190 = kk_string_empty(); /*string*/
    }
    _match3191: ;
  }
  _this_2133 = kk_std_os_path_split_base(_x3190, _ctx); /*(string, string)*/
  {
    kk_box_t _box_x2564 = _this_2133.fst;
    kk_box_t _box_x2565 = _this_2133.snd;
    kk_string_t _x = kk_string_unbox(_box_x2564);
    kk_string_dup(_x);
    kk_std_core_types__tuple2__drop(_this_2133, _ctx);
    return _x;
  }
}
 
// monadic lift


// lift anonymous function
struct kk_std_os_path__mlift2217_tempdir_fun3198__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path__mlift2217_tempdir_fun3198(kk_function_t _fself, kk_box_t _b_2568, kk_context_t* _ctx);
static kk_function_t kk_std_os_path__new_mlift2217_tempdir_fun3198(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path__mlift2217_tempdir_fun3198, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path__mlift2217_tempdir_fun3198(kk_function_t _fself, kk_box_t _b_2568, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3199;
  kk_string_t _x3200 = kk_string_unbox(_b_2568); /*string*/
  _x3199 = kk_std_os_path_path(_x3200, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3199, _ctx);
}

kk_std_os_path__path kk_std_os_path__mlift2217_tempdir(kk_string_t _y_2184, kk_context_t* _ctx) { /* (string) -> io path */ 
  kk_box_t _x3197 = kk_std_core_hnd__open_none1(kk_std_os_path__new_mlift2217_tempdir_fun3198(_ctx), kk_string_box(_y_2184), _ctx); /*1002*/
  return kk_std_os_path__path_unbox(_x3197, _ctx);
}
 
// Return the temporary directory for the current user.


// lift anonymous function
struct kk_std_os_path_tempdir_fun3202__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_tempdir_fun3202(kk_function_t _fself, kk_box_t _b_2572, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_tempdir_fun3202(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_tempdir_fun3202, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_tempdir_fun3202(kk_function_t _fself, kk_box_t _b_2572, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3203;
  kk_string_t _x3204 = kk_string_unbox(_b_2572); /*string*/
  _x3203 = kk_std_os_path__mlift2217_tempdir(_x3204, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3203, _ctx);
}


// lift anonymous function
struct kk_std_os_path_tempdir_fun3205__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_path_tempdir_fun3205(kk_function_t _fself, kk_box_t _b_2575, kk_context_t* _ctx);
static kk_function_t kk_std_os_path_new_tempdir_fun3205(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_path_tempdir_fun3205, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_path_tempdir_fun3205(kk_function_t _fself, kk_box_t _b_2575, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_os_path__path _x3206;
  kk_string_t _x3207 = kk_string_unbox(_b_2575); /*string*/
  _x3206 = kk_std_os_path_path(_x3207, _ctx); /*std/os/path/path*/
  return kk_std_os_path__path_box(_x3206, _ctx);
}

kk_std_os_path__path kk_std_os_path_tempdir(kk_context_t* _ctx) { /* () -> io path */ 
  kk_string_t x_2257 = kk_std_os_path_xtempdir(_ctx); /*string*/;
  kk_box_t _x3201;
  if (kk_yielding(kk_context())) {
    kk_string_drop(x_2257, _ctx);
    _x3201 = kk_std_core_hnd_yield_extend(kk_std_os_path_new_tempdir_fun3202(_ctx), _ctx); /*1002*/
  }
  else {
    _x3201 = kk_std_core_hnd__open_none1(kk_std_os_path_new_tempdir_fun3205(_ctx), kk_string_box(x_2257), _ctx); /*1002*/
  }
  return kk_std_os_path__path_unbox(_x3201, _ctx);
}

// initialization
void kk_std_os_path__init(kk_context_t* _ctx){
  static bool _kk_initialized = false;
  if (_kk_initialized) return;
  _kk_initialized = true;
  kk_std_core_types__init(_ctx);
  kk_std_core_hnd__init(_ctx);
  kk_std_core__init(_ctx);
  kk_std_text_parse__init(_ctx);
  #if defined(KK_CUSTOM_INIT)
    KK_CUSTOM_INIT (_ctx);
  #endif
}

// termination
void kk_std_os_path__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
  kk_std_text_parse__done(_ctx);
  kk_std_core__done(_ctx);
  kk_std_core_hnd__done(_ctx);
  kk_std_core_types__done(_ctx);
}
