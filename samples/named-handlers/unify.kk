/* Shows the use of named effect handlers under an umbrella effect.
   
   For more info see the paper:
   "First-class named effect handlers", Daan Leijen, Ningning Xie, and Youyou Cong, 2020.
*/
module unify

// A unifiable type
type utype<s>
  UVar( v : variable<s> )
  UCon( tag : string )
  UApp( t1 : utype<s>, t2 : utype<s> )
  
// A non-unifiable type
type ntype 
  Con( tag : string )
  App( t1 : ntype, t2 : ntype )


// Umbrella substitution effect
scoped effect subst<s::S> 
  ctl fresh() : variable<s>


// Unification variables under a substitution
named effect variable<s::S> in subst<s>    // named under umbrella effect `:subst`
  fun get() : maybe<utype<s>>              // `:(variable<s,a>)          -> <subst<s>,pure> maybe<utype<s>>`
  fun resolve( tp : utype<s> ) : ()        // `:(variable<s,a>,utype<s>) -> <subst<s>,pure> ()`


// private (named) handler instance for creating a unification variable
fun with-var(action) 
  var mtp := Nothing
  with v <- named handler 
    fun get() mtp
    fun resolve(tp) 
      match mtp
        Nothing -> 
          // if occurs(v,tp) then throw("recursive type")
          mtp := Just(tp) 
        Just    -> throw("cannot resolve a unification variable more than once")
  action(v)


// umbrella handler for substitution
fun subst(action : forall<s> () -> <subst<s>,pure|e> a) : <pure|e> a   // required rank-2 signature
  with ctl fresh() with-var(resume) 
  action()


// resolve all unification variables to a non-unifiable type
fun resolve-all( tp : utype<s> ) : <subst<s>,pure> ntype
  match tp
    UCon(tag)     -> Con(tag)
    UApp(tp1,tp2) -> App( resolve-all(tp1), resolve-all(tp2) )
    UVar(v) ->
      match get(v)
        Nothing   -> throw( "unresolved variable" )
        Just(tpv) -> resolve-all(tpv)


// Unify two types under a substitution handler
fun unify( tp1 : utype<s>, tp2 : utype<s> ) : <subst<s>,div,exn> utype<s>
  match (tp1,tp2) 
    (UCon(tag1), UCon(tag2)) | tag1 == tag2 -> tp1
    // (UVar(v1), UVar(v2))     | v1 == v2     -> tp1
    (UApp(tp11,tp12),UApp(tp21,tp22))       -> UApp( unify(tp11,tp21), unify(tp12,tp22) )
    (UVar(v1),_) -> match get(v1)
      Nothing -> 
        resolve(v1,tp2)
        tp2
      Just(tpv1) -> match tp2
        UVar(v2) -> match get(v2) 
          Nothing -> 
            resolve(v2,tpv1)
            tpv1
          Just(tpv2) 
            -> unify(tpv1,tpv2) 
        _ -> unify(tpv1,tp2)
    _ -> throw("cannot unify types")
 

// Helpers to create types
fun inttp() : utype<s> 
  UCon("int")

fun list( tp1 : utype<s> ) : utype<s>
  UApp( UCon("list"), tp1 )

fun to( tp1 : utype<s>, tp2 : utype<s> ) : utype<s>
  UApp( UApp( UCon("->"), tp1 ), tp2 )

pub fun show( tp : ntype, top : bool = True ) : string
  fun parens(s)
    if top then s else ("(" ++ s ++ ")" )
  match tp 
    App(App(Con("->"),t1),t2) -> (t1.show ++ " -> " ++ t2.show).parens
    Con(tag)   -> tag
    App(t1,t2) -> (t1.show ++ " " ++ t2.show(False)).parens


// Test unification
pub fun test()
  with subst
  val a = fresh()
  val b = fresh()
  val tp1 = to( UVar(a), UVar(a) )
  val tp2 = to( UVar(b), list(inttp()) )
  unify(tp1,tp2).resolve-all

pub fun main()
  val tp = test()
  println( "unified type: " ++ tp.show )

