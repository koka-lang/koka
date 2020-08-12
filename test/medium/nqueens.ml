open List;;

let rec safe queen diag xs = 
  match xs with
  | q :: qs -> queen <> q && queen <> q + diag && queen <> q - diag && safe queen (diag + 1) qs
  | [] -> true;;

let rec append_safe queen xs xss =
  if (queen <= 0) then xss
  else if (safe queen 1 xs) then append_safe (queen - 1) xs ((queen :: xs) :: xss)
  else append_safe (queen - 1) xs xss;;

let rec extend queen acc xss =
  match xss with
  | xs :: rest -> extend queen (append_safe queen xs acc) rest
  | [] -> acc;;

let rec find_solutions n queen =
  if (queen == 0) then [[]]
   else extend n [] (find_solutions n (queen - 1));;
   
let queens n = List.length (find_solutions n n);;

Printf.printf "%8d\n" (queens 13);;
