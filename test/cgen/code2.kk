fun id(x) {
  x
}

fun cons(x:a,y:b) :a {
  drop<b>(y)
  x
}

fun apply(f,x) {
  f(x)
}

fun map( xs : list<a>, f : a -> e b ) : list<b> {  
  match(xs) {        
    ys@Nil {
      drop(f)                       
      drop(ys)
      Nil            
    }
    ys@Cons(x0,xx@Cons(z,zz@Nil)) {      
      ----FromCore-----------
      x = ..
      xx = ..
      _z = ..
      ----------------
            .drop_pattern(ys,x,xx,_z);
            .orphan_pattern(ys,x,xx,_z);
              if (ys is unique) then {
                free(ys)
              }
              else {
                .dup(x)
                .dup(xx)
                .dup(_z)
                drop(ys)                
              }  
      -------------------
      drop(_z)
      val y  = f(x) 
      val yy = map( xx, f )          
      ys':Cons
      Cons(y,yy)@ys'                { y,yy } | Cons }
    }
  }  
}
