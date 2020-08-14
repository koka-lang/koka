// check copy constructors
struct tup2<a,b>( x : a, y : b )                       // value type
struct tup4<a,b,c,d>( x : a, y : b, z : c, zz : d)     // non-value type

fun copyx( t : tup4<a,b,c,d>, x : a = t.x, zz : d = t.zz ) : tup4<a,b,c,d> {
  Tup4(x,t.y,t.z,zz)  
}
