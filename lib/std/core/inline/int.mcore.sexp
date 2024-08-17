;; Converting from strings
(define $parseWithBase:(fun Pure (str int) ptr) (lambda ($s:str $base:int)
  (prim ($res:int $err:int) ("read(String, Int): Int" $s:str $base:int)
    (switch $err:int
      (1 ;; OK
         (make $std/core/types/maybe $std/core/types/Just ($res:top)))
      (_ ;; couldnt parse
         (make $std/core/types/maybe $std/core/types/Nothing ()))))))
(define $xparseImpl:(fun Pure (str int) ptr) (lambda ($s:str $hex:int)
  (switch $hex:int
    (0 ;; parse
       ($parseWithBase:(fun Pure (str int) ptr) $s:str 0)
    )
    (_ ;; hexadecimal
       ($parseWithBase:(fun Pure (str int) ptr) $s:str 16)))))

(unit)