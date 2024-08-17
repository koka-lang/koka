(define $"import$std/core/hnd":ptr (this-lib))

;; Current evidence vector
;; -----------------------
(define $getCurrentEvv:(fun Effectful () ptr) (lambda ()
  ("getRef(Ref[Ptr]): Ptr" ("getGlobal(String): Ptr" "current-evv")))
  :export-as ("getCurrentEvv"))
(define $setCurrentEvv:(fun Effectful (ptr) unit) (lambda ($evv:ptr)
  ("setRef(Ref[Ptr], Ptr): Unit" ("getGlobal(String): Ptr" "current-evv") $evv:ptr))
  :export-as ("setCurrentEvv"))
(define $swapCurrentEvv:(fun Effectful (ptr) ptr) (lambda ($evv:ptr)
  (letrec ((define $ref:ptr ("getGlobal(String): Ptr" "current-evv"))
           (define $old:ptr ("getRef(Ref[Ptr]): Ptr" $ref:ptr)))
    (begin
      ("setRef(Ref[Ptr], Ptr): Unit" $ref:ptr $evv:ptr)
      $old:ptr)))
  :export-as ("swapCurrentEvv"))
(define $evvSwapCreate1:(fun Effectful (int) ptr) (lambda ($n:int)
  (letrec ((define $cur:ptr ($getCurrentEvv:(fun Effectful () ptr)))
           (define $ev:ptr ($elt:top $cur:ptr $n:int))
           (define $next:ptr (make $evv $cons ($ev:ptr (make $evv $nil ())))))
    (begin
      ($setCurrentEvv:(fun Effectful (ptr) unit) $next:ptr)
      $cur:ptr)))
  :export-as ("evvSwapCreate1"))
(define $evvSwapCreate0:(fun Effectful () ptr) (lambda ()
  (letrec ((define $cur:ptr ($getCurrentEvv:(fun Effectful () ptr)))
           (define $next:ptr (make $evv $nil ())))
    (begin
      ($setCurrentEvv:(fun Effectful (ptr) unit) $next:ptr)
      $cur:ptr)))
  :export-as ("evvSwapCreate0"))
(define $evvSwapDelete:(fun Effectful (int int) ptr) (lambda ($i:int $behind:int)
  (letrec ((define $cur:ptr ($getCurrentEvv:(fun Effectful () ptr)))
           (define $next:ptr ($evvDelete:(fun Pure (int ptr) ptr) ("infixAdd(Int, Int): Int" $i:int $behind:int) $cur:ptr)))
    (begin
      ($setCurrentEvv:(fun Effectful (ptr) unit) $next:ptr)
      $cur:ptr)))
  :export-as ("evvSwapDelete"))
(define $evHtag:(fun Pure (ptr) str) (lambda ($ev:ptr)
  (project (project $ev:ptr $std/core/hnd/ev $std/core/hnd/Ev 0)
    $std/core/hnd/htag $std/core/hnd/Htag 0)))

;; make primitive?
(define $evvDelete:(fun Pure (int int ptr) ptr) (lambda ($i:int $evv:ptr)
  (match ($evv:ptr $evv)
    ($cons ($hd:ptr $tl:ptr)
      (switch $i:int
        (0 $tl:ptr)
        (_ (make $evv $cons (
              $hd:ptr
              ($evvDelete:(fun Pure (int int ptr) ptr) ("infixSub(Int, Int): Int" $i:int 1) $tl:ptr))))))
    (_ () ("panic(String): Bottom" "Out of bounds index into evidence vector"))))
  :export-as ("evvDelete"))
(define $evvInsert:(fun Pure (ptr ptr) ptr) (lambda ($evv:ptr $ev:ptr)
  (match ($evv:ptr $evv) 
     ($cons ($fst:ptr $rst:ptr)
        (switch ("infixGt(String, String): Boolean" 
                   ($evHtag:(fun Pure (ptr) str) $ev:ptr)
                   ($evHtag:(fun Pure (ptr) str) $fst:ptr))
          (1 (make $evv $cons (
                $fst:ptr
                ($evvInsert:(fun Pure (ptr ptr) ptr) $rst:ptr $ev:ptr))))
          (_ (make $evv $cons ($ev:ptr $evv:ptr)))))
     (_ () (make $evv $cons ($ev:ptr $evv:ptr)))))
  :export-as ("evvInsert"))
(define $evvIndex:(fun Pure (ptr ptr int) int) (lambda ($evv:ptr $htag:ptr $acc:int) ;; Find by htag
  (match ($evv:ptr $evv)
    ($cons ($fst:ptr $rst:ptr)
      (switch ("infixEq(String, String): Boolean"
                 (project $htag:ptr $std/core/hnd/htag $std/core/hnd/Htag 0)
                 ($evHtag:(fun Pure (ptr) str) $fst:ptr))
        (1 $acc:int)
        (_ ($evvIndex:(fun Pure (ptr ptr int) int) $rst:ptr $htag:ptr 
             ("infixAdd(Int, Int): Int" $acc:int 1)))))
    (_ () ("!undefined:no evidence for htag"))))
  :export-as ("evvIndex"))

;; List utilities
;; --------------
(define $elt:top (lambda ($l:ptr $n:int) 
  (switch $n:int 
    (0 (project $l:ptr $evv $cons 0)) 
    (_ ($elt:top (project $l:ptr $evv $cons 1) 
                 ("infixSub(Int, Int): Int" $n:int 1)))))
  :export-as ("elt"))

(unit)