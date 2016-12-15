data List a = Nil | Cons a (List a)

len xs
  = len' 0 xs

len' acc xs
  = case xs of
      Nil -> acc
      Cons _ t -> len' (acc+1) t

safe queen diag xs
  = case xs of
      Nil      -> True
      Cons q t -> queen /= q && queen /= q + diag && queen /= q - diag && safe queen (diag + 1) t

appendSafe k soln solns
  = if (k <= 0)
     then solns
     else if safe k 1 soln
           then appendSafe (k-1) soln (Cons (Cons k soln) solns)
           else appendSafe (k-1) soln solns


extend n acc solns
  = case solns of
      Nil            -> acc
      Cons soln rest -> extend n (appendSafe n soln acc) rest

find_solutions n k
  = if k == 0
     then Cons Nil Nil
     else extend n Nil (find_solutions n (k-1))

-- fst_solution n = head (find_solutions n n)

queens n
  = len (find_solutions n n)

main 
  = print (queens 13)
