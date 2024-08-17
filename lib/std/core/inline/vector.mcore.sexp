;; File with definitions for vectors (incomplete)
(define $vectorToList:top (lambda ($vec:ptr $tail:ptr $i:int)
  (switch $i:int
    (0 $tail:ptr)
    (_ (letrec ((define $ni:int ("infixSub(Int, Int): Int" $i:int 1))
                (define $el:ptr ("unsafeIndex(Array[Ptr], Int): Ptr" $vec:ptr $ni:int))
                (define $ntl:ptr (make $std/core/types/list $std/core/types/Cons 
                                       ($el:ptr $tail:ptr))))
         ($vectorToList:top $vec:ptr $ntl:ptr $ni:int))))))

(unit)