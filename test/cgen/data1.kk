/* DataEnum            -- only singletons
              | DataIso             -- only one constructor with one field
              | DataSingleStruct    -- only one constructor; it has  less than max-struct fields
              | DataSingle          -- only one constructor
              | DataAsList          -- one constructor with fields, and one singleton
              | DataSingleNormal    -- one constructor with fields, and possibly singletons
              | DataStruct          -- one constructor with non-recursive fields <= max-struct fields, and possibly singletons
              | DataNormal
              | DataOpen
*/

// enum
type void

// enum
type unit { Unit() }

// enum
type bool { False; True }

// iso
type iso { Iso(x:int) }

// single struct
type pair<a,b> { Pair(x:a, y:b) }

type triple<a,b,c> { Triple(p:pair<a,b>,z:c) }

// single
type single<a> { Single( x:int, y:a, z:bool ) }

// aslist
type list<a> { Cons(x:a,tail:list<a>); Nil }

// single normal
type single-normal<a> { Single-normal( x:a, y:maybe<int>, z:pair<bool,int> ); Single-normal-extra() }

// struct (as normal)
type vstrct<a> { VStrct( x:a, i:int ); VStrct2(d:float64,s:string); VStrct3(i:int32) }

// struct
type strct<a> { Strct( x:a, i:int ); Strct2(i:int, s:string); Strct3(x:a)  }

// struct
type maybe<a> { Just(x:a); Nothing }

// normal
type normal<a> { One(x:a,y:pair<int,int>,z:pair<int,int>); Two(x:int); Three }


// open
open type open<a> { Open1(:a) }

extend type open<a> { Open2(:int) }

// mixed raw / scan
type point { Point(x:float64,y:float64,z:int) }

// type wrong { Wrong( p1:point, p2:point ) }

// mix raw / scan
type mix<a> { Mix( p:point, m : mix<a>, i : int32, c : char ); MixNil }

// ensure the tag of maybe is in the scanned fields (3+1)
type scantag { ScanTag( i : int, z : char, m : maybe<int32>, p : point ) }
