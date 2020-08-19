module nqueens
import std/num/int32

alias solution = list<int32>
alias solutions = list<list<int32>>

fun safe( queen : int32, diag : int32, xs : solution ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag.inc,qs))
    _          -> True
  }
}

fun appendSafe( queen : int32, xs : solution, xss : solutions ) : div solutions {
  if (queen <= 0.int32) then xss
  elif (safe(queen,1.int32,xs)) then appendSafe( queen.dec, xs, Cons(Cons(queen,xs),xss) )
  else appendSafe( queen.dec, xs, xss )
}

fun extend(queen : int32, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, appendSafe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun findSolutions( n : int32, queen : int32 ) : div solutions {
  if (queen.zero?) 
   then [[]]
   else extend(n,[], findSolutions(n,queen.dec))  
}

public fun queens( n : int32 ) : div int {
  length(findSolutions(n,n))
}

public fun main() {
  queens(13.int32).println
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

let rec int32erval n m =
 if n > m then [] else n :: int32erval (n + 1) m;;

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
 let line = int32erval 1 size in
 let rec gen n size =
   if n = 0 then [[]] else
   concmap
    (fun b -> filter_append ok (map (fun q -> q :: b) line))
    (gen (n - 1) size) in
 gen size size;;

(* 2. Print32ing results. *)

let print32_solutions size solutions =
 let sol_num = ref 1 in
 iter
   (fun chess ->
     Print32f.print32f "\nSolution number %i\n" !sol_num;
     sol_num := !sol_num + 1;
     iter
       (fun line ->
         let count = ref 1 in
         while !count <= size do
           if !count = line then print32_string "Q " else print32_string "- ";
           count := !count + 1
         done;
         print32_newline ())
       chess)
   solutions;;

let print32_result size =
 let solutions = find_solutions size in
 let sol_num = List.length solutions in
 Print32f.print32f "The %i queens problem has %i solutions.\n" size sol_num;
 print32_newline ();
 let pr =
   print32_string "Do you want to see the solutions <n/y> ? "; read_line () in
 if pr = "y" then print32_solutions size solutions;;

(* 3. Main program. *)

let queens () =
 let size =
   print32_string "Chess boards's size ? "; read_int32 () in
 print32_result size;;

if !Sys.int32eractive then () else queens ();;
*/
