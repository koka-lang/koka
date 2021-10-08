#pragma once
#ifndef kk_std_os_path_H
#define kk_std_os_path_H
// Koka generated module: "std/os/path", koka version: 2.3.2, platform: 64-bit
#include <kklib.h>
#include "std_core_types.h"
#include "std_core_hnd.h"
#include "std_core.h"
#include "std_text_parse.h"

// type declarations

// value type std/os/path/path
struct kk_std_os_path_Path {
  kk_string_t root;
  kk_std_core__list parts;
};
typedef struct kk_std_os_path_Path kk_std_os_path__path;
static inline kk_std_os_path__path kk_std_os_path__new_Path(kk_string_t root, kk_std_core__list parts, kk_context_t* _ctx){
  kk_std_os_path__path _con;
  _con.root = root;
  _con.parts = parts;
  return _con;
}
static inline bool kk_std_os_path__is_Path(kk_std_os_path__path x) {
  return (true);
}
static inline kk_std_os_path__path kk_std_os_path__path_dup(kk_std_os_path__path _x) {
  kk_string_dup(_x.root);
  kk_std_core__list_dup(_x.parts);
  return _x;
}
static inline void kk_std_os_path__path_drop(kk_std_os_path__path _x, kk_context_t* _ctx) {
  kk_string_drop(_x.root, _ctx);
  kk_std_core__list_drop(_x.parts, _ctx);
}
static inline kk_box_t kk_std_os_path__path_box(kk_std_os_path__path _x, kk_context_t* _ctx) {
  kk_box_t _box;
  kk_valuetype_box(kk_std_os_path__path, _box, _x, 2 /* scan count */, _ctx);
  return _box;
}
static inline kk_std_os_path__path kk_std_os_path__path_unbox(kk_box_t _x, kk_context_t* _ctx) {
  kk_boxed_value_t _p;
  kk_std_os_path__path _unbox;
  kk_valuetype_unbox_(kk_std_os_path__path, _p, _unbox, _x, _ctx);
  if (_ctx!=NULL && _p!=NULL) {
    if (kk_basetype_is_unique(_p)) { kk_basetype_free(_p); } else {
      kk_std_os_path__path_dup(_unbox);
      kk_basetype_decref(_p, _ctx);
    }
  }
  return _unbox;
}

// value declarations
 
// Automatically generated. Retrieves the `root` constructor field of the `:path` type.

static inline kk_string_t kk_std_os_path_root(kk_std_os_path__path path0, kk_context_t* _ctx) { /* (path : path) -> string */ 
  {
    kk_string_t _x = path0.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(path0, _ctx);
    return _x;
  }
}
 
// Automatically generated. Retrieves the `parts` constructor field of the `:path` type.

static inline kk_std_core__list kk_std_os_path_parts(kk_std_os_path__path path0, kk_context_t* _ctx) { /* (path : path) -> list<string> */ 
  {
    kk_std_core__list _x = path0.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(path0, _ctx);
    return _x;
  }
}

kk_std_os_path__path kk_std_os_path__copy(kk_std_os_path__path _this, kk_std_core_types__optional root0, kk_std_core_types__optional parts0, kk_context_t* _ctx); /* (path, root : optional<string>, parts : optional<list<string>>) -> path */ 

kk_string_t kk_std_os_path_xapp_path(kk_context_t* _ctx); /* () -> io string */ 

kk_string_t kk_std_os_path_basename(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> string */ 

kk_std_os_path__path kk_std_os_path_nobase(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> path */ 

kk_std_core_types__tuple2_ kk_std_os_path_split_parts(kk_std_core__list parts0, kk_context_t* _ctx); /* (parts : list<string>) -> (string, list<string>) */ 

kk_string_t kk_std_os_path_xrealpath(kk_string_t p, kk_context_t* _ctx); /* (p : string) -> io string */ 

kk_string_t kk_std_os_path_dirname(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> string */ 
 
// Return a list of all directory components (excluding the root but including the basename).
// `"/foo/bar/test.txt".path.dirparts === ["foo","bar","test.txt"]`

static inline kk_std_core__list kk_std_os_path_dirparts(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> list<string> */ 
  kk_std_core__list _x2708;
  {
    kk_std_core__list _x = p.parts;
    kk_std_core__list_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    _x2708 = _x; /*list<string>*/
  }
  return kk_std_core__lift16747_reverse(kk_std_core__new_Nil(_ctx), _x2708, _ctx);
}

kk_string_t kk_std_os_path_xhomedir(kk_context_t* _ctx); /* () -> io string */ 

kk_std_os_path__path kk_std_os_path_nodir(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> path */ 

kk_string_t kk_std_os_path_parentname(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> string */ 

kk_string_t kk_std_os_path_partsep(kk_context_t* _ctx); /* () -> ndet string */ 

kk_string_t kk_std_os_path_pathsep(kk_context_t* _ctx); /* () -> ndet string */ 
 
// Return the root name of path.
// `"c:\\foo".path.rootname === "c:/"`
// `"/foo".path.rootname === "/"`

static inline kk_string_t kk_std_os_path_rootname(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  {
    kk_string_t _x = p.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    return _x;
  }
}

kk_string_t kk_std_os_path_xtempdir(kk_context_t* _ctx); /* () -> io string */ 

bool kk_std_os_path_is_empty(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> bool */ 

kk_std_os_path__path kk_std_os_path__lp__bar__bar__rp_(kk_std_os_path__path p1, kk_std_os_path__path p2, kk_context_t* _ctx); /* (p1 : path, p2 : path) -> path */ 

kk_std_core__list kk_std_os_path_push_part(kk_string_t dir, kk_std_core__list dirs, kk_context_t* _ctx); /* (dir : string, dirs : list<string>) -> list<string> */ 

kk_std_core__list kk_std_os_path_push_parts(kk_std_core__list parts0, kk_std_core__list dirs, kk_context_t* _ctx); /* (parts : list<string>, dirs : list<string>) -> list<string> */ 
 
// monadic lift

static inline bool kk_std_os_path__mlift2203_proot(kk_char_t wild__4, kk_context_t* _ctx) { /* (wild_4 : char) -> std/text/parse/parse bool */ 
  return false;
}
 
// monadic lift

static inline bool kk_std_os_path__mlift2204_proot(kk_unit_t wild__5, kk_context_t* _ctx) { /* (wild_5 : ()) -> std/text/parse/parse bool */ 
  return true;
}
 
// monadic lift

static inline kk_unit_t kk_std_os_path__mlift2205_proot(kk_char_t wild__0, kk_context_t* _ctx) { /* (wild_0 : char) -> std/text/parse/parse () */ 
  kk_Unit; return kk_Unit;
}

kk_unit_t kk_std_os_path__mlift2206_proot(kk_char_t wild__, kk_context_t* _ctx); /* (wild_ : char) -> std/text/parse/parse () */ 
 
// monadic lift

static inline kk_unit_t kk_std_os_path__mlift2207_proot(kk_std_core__list _y_2172, kk_context_t* _ctx) { /* (list<char>) -> std/text/parse/parse () */ 
  kk_std_core__list_drop(_y_2172, _ctx);
  kk_Unit; return kk_Unit;
}

kk_unit_t kk_std_os_path__mlift2208_proot(kk_char_t _y_2170, kk_context_t* _ctx); /* (char) -> std/text/parse/parse () */ 

kk_unit_t kk_std_os_path__mlift2209_proot(kk_char_t wild__1, kk_context_t* _ctx); /* (wild_1 : char) -> std/text/parse/parse () */ 

bool kk_std_os_path__mlift2210_proot(kk_unit_t wild__3, kk_context_t* _ctx); /* (wild_3 : ()) -> std/text/parse/parse bool */ 

bool kk_std_os_path_proot(kk_context_t* _ctx); /* () -> std/text/parse/parse bool */ 

kk_string_t kk_std_os_path_string(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> string */ 
 
// A `:path` represents a file system path.

static inline kk_std_os_path__path kk_std_os_path__create_Path(kk_std_core_types__optional root0, kk_std_core_types__optional parts0, kk_context_t* _ctx) { /* (root : optional<string>, parts : optional<list<string>>) -> path */ 
  kk_string_t _x2815;
  if (kk_std_core_types__is_Optional(root0)) {
    kk_box_t _box_x2350 = root0._cons.Optional.value;
    kk_string_t _root_871 = kk_string_unbox(_box_x2350);
    kk_string_dup(_root_871);
    kk_std_core_types__optional_drop(root0, _ctx);
    _x2815 = _root_871; /*string*/
    goto _match2816;
  }
  {
    _x2815 = kk_string_empty(); /*string*/
  }
  _match2816: ;
  kk_std_core__list _x2819;
  if (kk_std_core_types__is_Optional(parts0)) {
    kk_box_t _box_x2351 = parts0._cons.Optional.value;
    kk_std_core__list _parts_876 = kk_std_core__list_unbox(_box_x2351, NULL);
    kk_std_core__list_dup(_parts_876);
    kk_std_core_types__optional_drop(parts0, _ctx);
    _x2819 = _parts_876; /*list<string>*/
    goto _match2820;
  }
  {
    _x2819 = kk_std_core__new_Nil(_ctx); /*list<string>*/
  }
  _match2820: ;
  return kk_std_os_path__new_Path(_x2815, _x2819, _ctx);
}

kk_std_os_path__path kk_std_os_path_path_parts(kk_string_t root0, kk_string_t s, kk_std_core_types__optional dirs, kk_context_t* _ctx); /* (root : string, s : string, dirs : optional<list<string>>) -> path */ 

kk_std_os_path__path kk_std_os_path_path(kk_string_t s, kk_context_t* _ctx); /* (s : string) -> path */ 

kk_std_os_path__path kk_std_os_path__lp__fs__rp_(kk_std_os_path__path p1, kk_std_os_path__path p2, kk_context_t* _ctx); /* (p1 : path, p2 : path) -> path */ 

kk_std_os_path__path kk_std_os_path__lp__fs__1_rp_(kk_std_os_path__path p1, kk_string_t p2, kk_context_t* _ctx); /* (p1 : path, p2 : string) -> path */ 

kk_std_os_path__path kk_std_os_path__lp__fs__2_rp_(kk_string_t p1, kk_string_t p2, kk_context_t* _ctx); /* (p1 : string, p2 : string) -> path */ 

kk_std_os_path__path kk_std_os_path__mlift2211_app_path(kk_string_t _y_2178, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_app_path(kk_context_t* _ctx); /* () -> io path */ 

kk_std_os_path__path kk_std_os_path__mlift2212_appdir(kk_string_t _y_2179, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_appdir(kk_context_t* _ctx); /* () -> io path */ 

kk_std_os_path__path kk_std_os_path_change_base(kk_std_os_path__path p, kk_string_t basename0, kk_context_t* _ctx); /* (p : path, basename : string) -> path */ 

kk_std_core_types__tuple2_ kk_std_os_path_split_base(kk_string_t basename0, kk_context_t* _ctx); /* (basename : string) -> (string, string) */ 

kk_std_os_path__path kk_std_os_path_change_ext(kk_std_os_path__path p, kk_string_t extname0, kk_context_t* _ctx); /* (p : path, extname : string) -> path */ 

kk_string_t kk_std_os_path_extname(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> string */ 

kk_std_os_path__path kk_std_os_path_change_stem(kk_std_os_path__path p, kk_string_t stemname0, kk_context_t* _ctx); /* (p : path, stemname : string) -> path */ 

kk_std_os_path__path kk_std_os_path_combine(kk_std_core__list ps, kk_context_t* _ctx); /* (ps : list<path>) -> path */ 

kk_std_os_path__path kk_std_os_path__mlift2213_realpath_1(kk_string_t _y_2180, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_realpath_1(kk_string_t s, kk_context_t* _ctx); /* (s : string) -> io path */ 

kk_std_os_path__path kk_std_os_path__mlift2214_realpath(kk_string_t _y_2181, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_realpath(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> io path */ 

kk_std_os_path__path kk_std_os_path__mlift2215_cwd(kk_string_t _y_2182, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_cwd(kk_context_t* _ctx); /* () -> io path */ 

kk_std_os_path__path kk_std_os_path_default_ext(kk_std_os_path__path p, kk_string_t newext, kk_context_t* _ctx); /* (p : path, newext : string) -> path */ 

kk_std_os_path__path kk_std_os_path__mlift2216_homedir(kk_string_t _y_2183, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_homedir(kk_context_t* _ctx); /* () -> io path */ 
 
// Is a path relative?

static inline bool kk_std_os_path_is_relative(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> bool */ 
  kk_string_t _x3142;
  {
    kk_string_t _x = p.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    _x3142 = _x; /*string*/
  }
  kk_string_t _x3143 = kk_string_empty(); /*string*/
  return kk_string_is_eq(_x3142,_x3143,kk_context());
}
 
// Is a path absolute?

static inline bool kk_std_os_path_is_absolute(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> bool */ 
  bool b_2122;
  kk_string_t _x3145;
  {
    kk_string_t _x = p.root;
    kk_string_dup(_x);
    kk_std_os_path__path_drop(p, _ctx);
    _x3145 = _x; /*string*/
  }
  kk_string_t _x3146 = kk_string_empty(); /*string*/
  b_2122 = kk_string_is_eq(_x3145,_x3146,kk_context()); /*bool*/
  if (b_2122) {
    return false;
  }
  {
    return true;
  }
}
 
// Remove the extension from a path.

static inline kk_std_os_path__path kk_std_os_path_noext(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> path */ 
  kk_string_t _x3148 = kk_string_empty(); /*string*/
  return kk_std_os_path_change_ext(p, _x3148, _ctx);
}

kk_std_core__list kk_std_os_path__ctail_paths_collect(kk_std_core__list ps, kk_std_core_types__ctail _acc, kk_context_t* _ctx); /* (ps : list<string>, ctail<list<path>>) -> list<path> */ 

kk_std_core__list kk_std_os_path_paths_collect(kk_std_core__list ps0, kk_context_t* _ctx); /* (ps : list<string>) -> list<path> */ 
 
// Parse a list of paths seperated by colon (`':'`) or semi-colon (`';'`)
//
// Colon separated paths can be ambiguous with Windows style root names (`c:\\`)
// In particular, a single letter path followed by an absolute path, e.g. ``c:/foo:/bar`` is
// parsed as ``c:/foo`` and ``/bar``.

static inline kk_std_core__list kk_std_os_path_paths(kk_string_t s, kk_context_t* _ctx) { /* (s : string) -> list<path> */ 
  kk_std_core__list _x3182;
  kk_string_t s0_2131;
  kk_string_t _x3183;
  kk_define_string_literal(, _s3184, 1, ";")
  _x3183 = kk_string_dup(_s3184); /*string*/
  kk_string_t _x3185;
  kk_define_string_literal(, _s3186, 1, ":")
  _x3185 = kk_string_dup(_s3186); /*string*/
  s0_2131 = kk_string_replace_all(s,_x3183,_x3185,kk_context()); /*string*/
  kk_vector_t v_16666;
  kk_string_t _x3187;
  kk_define_string_literal(, _s3188, 1, ":")
  _x3187 = kk_string_dup(_s3188); /*string*/
  v_16666 = kk_string_splitv(s0_2131,_x3187,kk_context()); /*vector<string>*/
  _x3182 = kk_std_core_vlist(v_16666, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
  return kk_std_os_path_paths_collect(_x3182, _ctx);
}
 
// Show a path as a string.

static inline kk_string_t kk_std_os_path_show(kk_std_os_path__path p, kk_context_t* _ctx) { /* (p : path) -> string */ 
  kk_string_t _x3189 = kk_std_os_path_string(p, _ctx); /*string*/
  return kk_std_core_show_3(_x3189, _ctx);
}

kk_string_t kk_std_os_path_stemname(kk_std_os_path__path p, kk_context_t* _ctx); /* (p : path) -> string */ 

kk_std_os_path__path kk_std_os_path__mlift2217_tempdir(kk_string_t _y_2184, kk_context_t* _ctx); /* (string) -> io path */ 

kk_std_os_path__path kk_std_os_path_tempdir(kk_context_t* _ctx); /* () -> io path */ 

void kk_std_os_path__init(kk_context_t* _ctx);


void kk_std_os_path__done(kk_context_t* _ctx);

#endif // header
