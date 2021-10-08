// Koka generated module: "std/os/env", koka version: 2.3.2, platform: 64-bit
#include "std_os_env.h"

kk_vector_t kk_std_os_env_os_get_argv(kk_context_t* _ctx) { /* () -> ndet vector<string> */ 
  return kk_os_get_argv(kk_context());
}

kk_vector_t kk_std_os_env_os_get_env(kk_context_t* _ctx) { /* () -> ndet vector<string> */ 
  return kk_os_get_env(kk_context());
}
 
// The backend compiler name.

kk_string_t kk_std_os_env_get_cc_name(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_cc_name(kk_context());
}
 
// The current compiler version.

kk_string_t kk_std_os_env_get_compiler_version(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_compiler_version(kk_context());
}
 
// Return the processor maximum address size in bits (`8*sizeof(void*)`). This is usually
// equal to the `get-cpu-arch-bits` but may be different on segmented architectures.

kk_integer_t kk_std_os_env_get_cpu_address_bits(kk_context_t* _ctx) { /* () -> ndet int */ 
  return kk_integer_from_size_t(CHAR_BIT*sizeof(void*),kk_context());
}
 
// Return the main processor architecture: x64, x86, arm64, arm, riscv32, riscv64, alpha, ppc64, etc.

kk_string_t kk_std_os_env_get_cpu_arch(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_cpu_arch(kk_context());
}
 
// Return the processor architecture natural machine word size in bits.
//
// Note: Usually this equals the `get-cpu-object-bits` and `get-cpu-address-bits` on modern cpu's
// but they can differ on segmented architectures.
// For example, on the old x86 FAR-NEAR model, the addresses are 32-bit but the maximum object size is 16-bit.
// Or on the more recent-[x32 ABI](https://en.wikipedia.org/wiki/X32_ABI)
// the addresses and objects are 32-bits but the architecture has 64-bit registers.

kk_integer_t kk_std_os_env_get_cpu_arch_bits(kk_context_t* _ctx) { /* () -> ndet int */ 
  return kk_integer_from_size_t(CHAR_BIT*(sizeof(size_t) > sizeof(long) ? sizeof(size_t) : sizeof(long)),kk_context());
}
 
// Return the available CPU's.
// This is the logical core count including hyper-threaded cores.

kk_integer_t kk_std_os_env_get_cpu_count(kk_context_t* _ctx) { /* () -> ndet int */ 
  return kk_integer_from_int(kk_cpu_count(kk_context()),kk_context());
}
 
// Is the byte-order little-endian?
// If not, it is big-endian; other byte orders are not supported.

bool kk_std_os_env_get_cpu_is_little_endian(kk_context_t* _ctx) { /* () -> ndet bool */ 
  return kk_cpu_is_little_endian(kk_context());
}
 
// Return the processor maximum object size in bits (`8*sizeof(size_t)`). This is usually
// equal to the `get-cpu-arch-bits` but may be different on segmented architectures.

kk_integer_t kk_std_os_env_get_cpu_object_bits(kk_context_t* _ctx) { /* () -> ndet int */ 
  return kk_integer_from_size_t(CHAR_BIT*sizeof(size_t),kk_context());
}
 
// Return the main OS name: windows, linux, macos, unix, posix, ios, tvos, watchos, unknown.
// Sometimes has a _dash_ subsystem, like: unix-\<freebsd,openbsd,dragonfly,bsd\>, and windows-mingw.

kk_string_t kk_std_os_env_get_os_name(kk_context_t* _ctx) { /* () -> ndet string */ 
  return kk_os_name(kk_context());
}


// lift anonymous function
struct kk_std_os_env_argv_fun439__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_env_argv_fun439(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_env_new_argv_fun439(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_env_argv_fun439, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_env_argv_fun439(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core__list _x440;
  kk_vector_t v_355 = kk_std_os_env_os_get_argv(_ctx); /*vector<string>*/;
  _x440 = kk_std_core_vlist(v_355, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
  return kk_std_core__list_box(_x440, _ctx);
}

kk_std_core__delayed kk_std_os_env_argv;

kk_std_core__list kk_std_os_env__ctail_to_tuples(kk_std_core__list xs, kk_std_core_types__ctail _acc, kk_context_t* _ctx) { /* (xs : list<string>, ctail<env>) -> env */ 
  kk__tailcall: ;
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con441 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x365 = _con441->head;
    kk_std_core__list _pat0 = _con441->tail;
    if (kk_std_core__is_Cons(_pat0)) {
      struct kk_std_core_Cons* _con443 = kk_std_core__as_Cons(_pat0);
      kk_string_t name = kk_string_unbox(_box_x365);
      kk_box_t _box_x366 = _con443->head;
      kk_std_core__list xx = _con443->tail;
      kk_string_t value = kk_string_unbox(_box_x366);
      kk_reuse_t _ru_431 = kk_reuse_null; /*reuse*/;
      if (kk_likely(kk_std_core__list_is_unique(xs))) {
        if (kk_likely(kk_std_core__list_is_unique(_pat0))) {
          _ru_431 = (kk_std_core__list_reuse(_pat0));
        }
        else {
          kk_string_dup(value);
          kk_std_core__list_dup(xx);
          kk_std_core__list_decref(_pat0, _ctx);
          _ru_431 = kk_reuse_null;
        }
        kk_std_core__list_free(xs);
      }
      else {
        kk_string_dup(name);
        kk_string_dup(value);
        kk_std_core__list_dup(xx);
        _ru_431 = kk_reuse_null;
        kk_std_core__list_decref(xs, _ctx);
      }
      kk_std_core__list _ctail_361 = kk_std_core__list_hole(); /*std/os/env/env*/;
      kk_std_core__list _ctail_362;
      kk_box_t _x445;
      kk_std_core_types__tuple2_ _x446 = kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(name), kk_string_box(value), _ctx); /*(1004, 1005)*/
      _x445 = kk_std_core_types__tuple2__box(_x446, _ctx); /*1009*/
      _ctail_362 = kk_std_core__new_Cons(_ru_431, _x445, _ctail_361, _ctx); /*list<(string, string)>*/
      { // tailcall
        kk_std_core_types__ctail _x447;
        kk_box_t* _b_392_380 = (kk_box_t*)((&kk_std_core__as_Cons(_ctail_362)->tail)); /*cfield<std/os/env/env>*/;
        _x447 = kk_ctail_link(_acc,(kk_std_core__list_box(_ctail_362, _ctx)),_b_392_380); /*ctail<0>*/
        xs = xx;
        _acc = _x447;
        goto kk__tailcall;
      }
    }
  }
  if (kk_std_core__is_Cons(xs)) {
    struct kk_std_core_Cons* _con448 = kk_std_core__as_Cons(xs);
    kk_box_t _box_x381 = _con448->head;
    kk_std_core__list _pat2 = _con448->tail;
    kk_string_t name0 = kk_string_unbox(_box_x381);
    kk_reuse_t _ru_432 = kk_reuse_null; /*reuse*/;
    if (kk_likely(kk_std_core__list_is_unique(xs))) {
      _ru_432 = (kk_std_core__list_reuse(xs));
    }
    else {
      kk_string_dup(name0);
      kk_std_core__list_decref(xs, _ctx);
      _ru_432 = kk_reuse_null;
    }
    kk_box_t _x450;
    kk_box_t _x451;
    kk_std_core__list _x452;
    if (kk_likely(_ru_432!=NULL)) {
      kk_std_core_types__tuple2_ _x453;
      kk_box_t _x454;
      kk_string_t _x455 = kk_string_empty(); /*string*/
      _x454 = kk_string_box(_x455); /*1005*/
      _x453 = kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(name0), _x454, _ctx); /*(1004, 1005)*/
      struct kk_std_core_Cons* _con457 = (struct kk_std_core_Cons*)_ru_432;
      _con457->head = kk_std_core_types__tuple2__box(_x453, _ctx);
      _x452 = kk_std_core__base_Cons(_con457); /*list<1009>*/
    }
    else {
      kk_box_t _x458;
      kk_std_core_types__tuple2_ _x459;
      kk_box_t _x460;
      kk_string_t _x461 = kk_string_empty(); /*string*/
      _x460 = kk_string_box(_x461); /*1005*/
      _x459 = kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(name0), _x460, _ctx); /*(1004, 1005)*/
      _x458 = kk_std_core_types__tuple2__box(_x459, _ctx); /*1009*/
      _x452 = kk_std_core__new_Cons(kk_reuse_null, _x458, _pat2, _ctx); /*list<1009>*/
    }
    _x451 = kk_std_core__list_box(_x452, _ctx); /*-1*/
    _x450 = kk_ctail_resolve(_acc,_x451); /*-1*/
    return kk_std_core__list_unbox(_x450, _ctx);
  }
  {
    kk_box_t _x463 = kk_ctail_resolve(_acc,(kk_std_core__list_box(kk_std_core__new_Nil(_ctx), _ctx))); /*-1*/
    return kk_std_core__list_unbox(_x463, _ctx);
  }
}

kk_std_core__list kk_std_os_env_to_tuples(kk_std_core__list xs0, kk_context_t* _ctx) { /* (xs : list<string>) -> env */ 
  kk_std_core_types__ctail _x464 = kk_ctail_nil(); /*ctail<0>*/
  return kk_std_os_env__ctail_to_tuples(xs0, _x464, _ctx);
}


// lift anonymous function
struct kk_std_os_env_environ_fun465__t {
  struct kk_function_s _base;
};
static kk_box_t kk_std_os_env_environ_fun465(kk_function_t _fself, kk_context_t* _ctx);
static kk_function_t kk_std_os_env_new_environ_fun465(kk_context_t* _ctx) {
  kk_define_static_function(_fself, kk_std_os_env_environ_fun465, _ctx)
  return kk_function_dup(_fself);
}

static kk_box_t kk_std_os_env_environ_fun465(kk_function_t _fself, kk_context_t* _ctx) {
  KK_UNUSED(_fself);
  kk_std_core__list _x466;
  kk_std_core__list _x467;
  kk_vector_t v_356 = kk_std_os_env_os_get_env(_ctx); /*vector<string>*/;
  _x467 = kk_std_core_vlist(v_356, kk_std_core_types__new_None(_ctx), _ctx); /*list<1001>*/
  _x466 = kk_std_os_env_to_tuples(_x467, _ctx); /*std/os/env/env*/
  return kk_std_core__list_box(_x466, _ctx);
}

kk_std_core__delayed kk_std_os_env_environ;
 
// Return the arguments that were passed to program itself.
// Strips off the initial program from the unprocessed command line.
// i.e. If a program started as:
// ````
// > node myprogram.js --flag bla
// ````
// The `arguments` list will be `["--flag","bla"]`

kk_std_core__list kk_std_os_env_get_args(kk_context_t* _ctx) { /* () -> ndet list<string> */ 
  bool is_node;
  kk_string_t _x470 = kk_std_core_host(_ctx); /*string*/
  kk_string_t _x471;
  kk_define_string_literal(, _s472, 4, "node")
  _x471 = kk_string_dup(_s472); /*string*/
  is_node = kk_string_is_eq(_x470,_x471,kk_context()); /*bool*/
  kk_std_core__list _match_438;
  kk_box_t _x473;
  kk_std_core__delayed _x474 = kk_std_core__delayed_dup(kk_std_os_env_argv); /*delayed<ndet,list<string>>*/
  _x473 = kk_std_core_force(_x474, _ctx); /*1001*/
  _match_438 = kk_std_core__list_unbox(_x473, _ctx); /*list<string>*/
  if (kk_std_core__is_Cons(_match_438)) {
    struct kk_std_core_Cons* _con475 = kk_std_core__as_Cons(_match_438);
    kk_box_t _box_x409 = _con475->head;
    kk_std_core__list xx = _con475->tail;
    kk_string_t x = kk_string_unbox(_box_x409);
    bool _x477;
    if (is_node) {
      kk_string_t _x478;
      kk_std_os_path__path p_357;
      kk_string_t _x479 = kk_string_dup(x); /*string*/
      p_357 = kk_std_os_path_path(_x479, _ctx); /*std/os/path/path*/
      kk_std_core_types__tuple2_ _this_2133;
      kk_string_t _x480;
      {
        kk_std_core__list _x0 = p_357.parts;
        kk_std_core__list_dup(_x0);
        kk_std_os_path__path_drop(p_357, _ctx);
        if (kk_std_core__is_Cons(_x0)) {
          struct kk_std_core_Cons* _con482 = kk_std_core__as_Cons(_x0);
          kk_box_t _box_x410 = _con482->head;
          kk_std_core__list _pat00 = _con482->tail;
          kk_string_t x0 = kk_string_unbox(_box_x410);
          if (kk_likely(kk_std_core__list_is_unique(_x0))) {
            kk_std_core__list_drop(_pat00, _ctx);
            kk_std_core__list_free(_x0);
          }
          else {
            kk_string_dup(x0);
            kk_std_core__list_decref(_x0, _ctx);
          }
          _x480 = x0; /*string*/
          goto _match481;
        }
        {
          _x480 = kk_string_empty(); /*string*/
        }
        _match481: ;
      }
      _this_2133 = kk_std_os_path_split_base(_x480, _ctx); /*(string, string)*/
      {
        kk_box_t _box_x411 = _this_2133.fst;
        kk_box_t _box_x412 = _this_2133.snd;
        kk_string_t _x = kk_string_unbox(_box_x411);
        kk_string_dup(_x);
        kk_std_core_types__tuple2__drop(_this_2133, _ctx);
        _x478 = _x; /*string*/
      }
      kk_string_t _x487;
      kk_define_string_literal(, _s488, 4, "node")
      _x487 = kk_string_dup(_s488); /*string*/
      _x477 = kk_string_is_eq(_x478,_x487,kk_context()); /*bool*/
    }
    else {
      _x477 = false; /*bool*/
    }
    if (_x477) {
      if (kk_likely(kk_std_core__list_is_unique(_match_438))) {
        kk_box_drop(_box_x409, _ctx);
        kk_std_core__list_free(_match_438);
      }
      else {
        kk_std_core__list_dup(xx);
        kk_std_core__list_decref(_match_438, _ctx);
      }
      return kk_std_core_drop(xx, kk_integer_from_small(1), _ctx);
    }
  }
  {
    return kk_std_core_drop(_match_438, kk_integer_from_small(1), _ctx);
  }
}
 
// Returns the value of an environment variable `name` (or `Nothing` if not present)


// lift anonymous function
struct kk_std_os_env_get_env_fun493__t_1 {
  struct kk_function_s _base;
  kk_string_t name;
};
static kk_std_core_types__maybe kk_std_os_env_get_env_fun493_1(kk_function_t _fself, kk_box_t _b_425, kk_context_t* _ctx);
static kk_function_t kk_std_os_env_new_get_env_fun493_1(kk_string_t name, kk_context_t* _ctx) {
  struct kk_std_os_env_get_env_fun493__t_1* _self = kk_function_alloc_as(struct kk_std_os_env_get_env_fun493__t_1, 2, _ctx);
  _self->_base.fun = kk_cfun_ptr_box(&kk_std_os_env_get_env_fun493_1, kk_context());
  _self->name = name;
  return &_self->_base;
}

static kk_std_core_types__maybe kk_std_os_env_get_env_fun493_1(kk_function_t _fself, kk_box_t _b_425, kk_context_t* _ctx) {
  struct kk_std_os_env_get_env_fun493__t_1* _self = kk_function_as(struct kk_std_os_env_get_env_fun493__t_1*, _fself);
  kk_string_t name = _self->name; /* string */
  kk_drop_match(_self, {kk_string_dup(name);}, {}, _ctx)
  bool _match_435;
  kk_string_t _x494;
  kk_std_core_types__tuple2_ _match_437;
  kk_box_t _x495 = kk_box_dup(_b_425); /*1001*/
  _match_437 = kk_std_core_types__tuple2__unbox(_x495, _ctx); /*(string, string)*/
  {
    kk_box_t _box_x418 = _match_437.fst;
    kk_box_t _box_x419 = _match_437.snd;
    kk_string_t _x = kk_string_unbox(_box_x418);
    kk_string_dup(_x);
    kk_std_core_types__tuple2__drop(_match_437, _ctx);
    _x494 = _x; /*string*/
  }
  _match_435 = kk_string_is_eq(_x494,name,kk_context()); /*bool*/
  if (_match_435) {
    kk_box_t _x498;
    kk_string_t _x499;
    kk_std_core_types__tuple2_ _match_436 = kk_std_core_types__tuple2__unbox(_b_425, _ctx); /*(string, string)*/;
    {
      kk_box_t _box_x420 = _match_436.fst;
      kk_box_t _box_x421 = _match_436.snd;
      kk_string_t _x0 = kk_string_unbox(_box_x421);
      kk_string_dup(_x0);
      kk_std_core_types__tuple2__drop(_match_436, _ctx);
      _x499 = _x0; /*string*/
    }
    _x498 = kk_string_box(_x499); /*1034*/
    return kk_std_core_types__new_Just(_x498, _ctx);
  }
  {
    kk_box_drop(_b_425, _ctx);
    return kk_std_core_types__new_Nothing(_ctx);
  }
}

kk_std_core_types__maybe kk_std_os_env_get_env_1(kk_string_t name, kk_context_t* _ctx) { /* (name : string) -> ndet maybe<string> */ 
  kk_std_core__list xs_358;
  kk_box_t _x491;
  kk_std_core__delayed _x492 = kk_std_core__delayed_dup(kk_std_os_env_environ); /*delayed<ndet,std/os/env/env>*/
  _x491 = kk_std_core_force(_x492, _ctx); /*1001*/
  xs_358 = kk_std_core__list_unbox(_x491, _ctx); /*list<(string, string)>*/
  return kk_std_core_foreach_while(xs_358, kk_std_os_env_new_get_env_fun493_1(name, _ctx), _ctx);
}

// initialization
void kk_std_os_env__init(kk_context_t* _ctx){
  static bool _kk_initialized = false;
  if (_kk_initialized) return;
  _kk_initialized = true;
  kk_std_core_types__init(_ctx);
  kk_std_core__init(_ctx);
  kk_std_os_path__init(_ctx);
  #if defined(KK_CUSTOM_INIT)
    KK_CUSTOM_INIT (_ctx);
  #endif
  {
    kk_std_os_env_argv = kk_std_core_delay(kk_std_os_env_new_argv_fun439(_ctx), _ctx); /*delayed<ndet,list<string>>*/
  }
  {
    kk_std_os_env_environ = kk_std_core_delay(kk_std_os_env_new_environ_fun465(_ctx), _ctx); /*delayed<ndet,std/os/env/env>*/
  }
}

// termination
void kk_std_os_env__done(kk_context_t* _ctx){
  static bool _kk_done = false;
  if (_kk_done) return;
  _kk_done = true;
  #if defined(KK_CUSTOM_DONE)
    KK_CUSTOM_DONE (_ctx);
  #endif
  kk_std_core__delayed_drop(kk_std_os_env_environ, _ctx);
  kk_std_core__delayed_drop(kk_std_os_env_argv, _ctx);
  kk_std_os_path__done(_ctx);
  kk_std_core__done(_ctx);
  kk_std_core_types__done(_ctx);
}
