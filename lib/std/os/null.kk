// Used for external interfaces
module std/os/null

// Abstract type used for passing `null` values to external functions
pub type null<a>

// Unsafe: transform any type to a `null` type; used internally by the compiler.
pub extern @null-any(x : a) : null<a>
  c  inline "(kk_box_is_null(#1) ? kk_datatype_null() : kk_datatype_unbox(#1))"
  cs inline "#1"
  js inline "(#1==null ? null : #1)"  // undefined -> null

// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).
pub extern maybe/null(x : maybe<a>) : null<a>
  c  inline "(kk_std_core_types__is_Nothing(#1,kk_context()) ? kk_datatype_null() : kk_datatype_unbox((#1)._cons.Just.value) /* kk_datatype_unbox(kk_datatype_unjust(#1,kk_context())) */ )"
  cs inline "(#1.tag_ == __std_core._maybe_Tag.Nothing ? default(##1) : #1.@value)"
  js inline "(#1==null ? null : #1.value)"

// Transform a `:null` type to a `:maybe` type. Note that it is not
// always the case that `id(x) == maybe(null(x))` (e.g. when `x = Just(Nothing)`).
pub extern null/maybe( n : null<a> ) : maybe<a>
  c  inline "(kk_datatype_is_null(#1) ? kk_std_core_types__new_Nothing(kk_context()) : kk_std_core_types__new_Just(kk_datatype_box(#1),kk_context()))"
  cs inline "(EqualityComparer<##1>.Default.Equals(#1,default(##1)) ? __std_core._maybe<##1>.Nothing_ : new __std_core._maybe<##1>(#1))"
  js inline "(#1==null ? $std_core_types.Nothing : $std_core_types.Just(#1))"

// Cast a integer that is zero to a null
pub fun int/null( i : int ) : null<int>
    i.maybe.null

// Cast an empty string a null
pub fun string/null( s : string ) : null<string>
    s.maybe.null

// Cast a boolean `False` to null
pub fun bool/null( b : bool ) : null<()>
    if b then Just(()).null else Nothing.null

// val null-const : forall<a> null<a> = null(Nothing)
