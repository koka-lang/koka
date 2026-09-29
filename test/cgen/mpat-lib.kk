module mpat-lib

// Helper for mpat.kk: an inlineable function with a multi-scrutinee match,
// whose branch patterns must keep their order through the .kki inline section.
pub type dir
  North
  East(i : int)
  South
  West(i : int)

pub fun cross( d1 : dir, d2 : dir ) : bool
  match (d1, d2)
    (North, _)          -> True
    (East(_), East(_))  -> True
    _                   -> False

pub fun main()
  println( cross(North, South) )
