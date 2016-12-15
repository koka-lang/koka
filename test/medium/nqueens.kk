module nqueens

alias solution = list<int>
alias solutions = list<list<int>>

fun safe( queen : int, diag : int, xs : solution ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag+1,qs))
    _          -> True
  }
}

fun appendSafe( queen : int, xs : solution, xss : solutions ) : div solutions {
  if (queen <= 0) then xss
  elif (safe(queen,1,xs)) then appendSafe( queen - 1, xs, Cons(Cons(queen,xs),xss) )
  else appendSafe( queen - 1, xs, xss )
}

fun extend(queen : int, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, appendSafe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun findSolutions( n : int, queen : int ) : div solutions {
  if (queen == 0) 
   then [[]]
   else extend(n,[], findSolutions(n,queen - 1))  
}

public fun queens( n : int ) : div int {
  length(findSolutions(n,n))
}

public fun main() {
  queens(8).println
}


/*
-- remove the following comment to get a Haskell program
-- data List a = Nil | Cons a (List a)
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
  = if (k `lt` 0)
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

fst_solution n = head (find_solutions n n)

queens n
  = len (find_solutions n n)
*/

/*
open List;;

let rec interval n m =
 if n > m then [] else n :: interval (n + 1) m;;

let filter_append p l l0 =
  let rec filter = fun
    | [] -> l0
    | h :: t -> if p h then h :: filter t else filter t in
   filter l;;

let rec concmap f = fun
  | [] -> []
  | h :: t -> f h (concmap f t);;

let rec safe x d  = fun
  | [] -> true
  | h :: t ->
     x <> h && x <> h + d && x <> h - d && safe x (d + 1) t;;

let rec ok = fun
  | [] -> true
  | h :: t -> safe h 1 t;;

let find_solutions size =
 let line = interval 1 size in
 let rec gen n size =
   if n = 0 then [[]] else
   concmap
    (fun b -> filter_append ok (map (fun q -> q :: b) line))
    (gen (n - 1) size) in
 gen size size;;

(* 2. Printing results. *)

let print_solutions size solutions =
 let sol_num = ref 1 in
 iter
   (fun chess ->
     Printf.printf "\nSolution number %i\n" !sol_num;
     sol_num := !sol_num + 1;
     iter
       (fun line ->
         let count = ref 1 in
         while !count <= size do
           if !count = line then print_string "Q " else print_string "- ";
           count := !count + 1
         done;
         print_newline ())
       chess)
   solutions;;

let print_result size =
 let solutions = find_solutions size in
 let sol_num = List.length solutions in
 Printf.printf "The %i queens problem has %i solutions.\n" size sol_num;
 print_newline ();
 let pr =
   print_string "Do you want to see the solutions <n/y> ? "; read_line () in
 if pr = "y" then print_solutions size solutions;;

(* 3. Main program. *)

let queens () =
 let size =
   print_string "Chess boards's size ? "; read_int () in
 print_result size;;

if !Sys.interactive then () else queens ();;
*/
