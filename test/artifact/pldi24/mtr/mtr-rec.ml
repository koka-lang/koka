open Int32

type tree =
| Leaf
| Node of tree * int * tree;;

type root =
| Root of tree * int * tree;;


let rec accessz t k =
  match t with
  | Node (l,x,r) -> if (x < k) then
                      match accessz r k with
                      | Root (s, f, b) -> Root (Node (l, x, s), f, b)
                    else if (x > k) then 
                      match accessz l k with
                      | Root (s, f, b) -> Root (s, f, Node (b, x, r))
                    else Root (l, x, r)
  | Leaf         -> Root (Leaf, k, Leaf);;

  let access t k =
    match accessz t k with
    | Root (s, f, b) -> Node (s, f, b);;  


(* helpers *)

let rec sumacc t acc = 
  match t with
  | Node (l,x,r) -> sumacc r (sumacc l (acc+x))
  | Leaf         -> acc;;

let sum t = sumacc t 0;;


let imin x y = if (x <= y) then x else y;;
let imax x y = if (x >= y) then x else y;;

let rec minheight t = 
  match t with
  | Node (l,x,r) -> 1 + imin (minheight l) (minheight r)
  | Leaf             -> 0;;

let rec maxheight t = 
  match t with
  | Node (l,x,r) -> 1 + imax (maxheight l) (maxheight r)
  | Leaf             -> 0;;

let top t = 
  match t with
  | Node (l,x,r) -> x
  | Leaf             -> 0;;


type sfc =
| Sfc of int32 * int32 * int32 * int32;;

let rotl i n =
  logor (shift_left i n) (shift_right_logical i (32 - n));;

let sfc_step sfc =
  match sfc with
  | Sfc (x,y,z,cnt) -> 
    let res = add x  (add y cnt) in
    (to_int res,Sfc (logxor y (shift_right_logical y 9),
                      add z (shift_left z 3),
                      add (rotl z 21)  res,
                      add cnt one ));;

let sfc_init seed1 seed2 =
  let s = ref (Sfc (zero,of_int seed1, of_int seed2,one)) in
  for i = 1 to 12 do
    let (_,s1) = sfc_step !s in
    s := s1
  done;
  !s;;


let modE i j =
  let m = i mod j in
  if (i < 0 && m < 0) then (if (j < 0) then m - j else m + j)
  else m;;

let test n iter =
  let t = ref Leaf in
  let s = ref (sfc_init 42 43) in
  for i = 1 to iter do
    for j = 1 to n do
      let (x,s1) = sfc_step !s in
      s := s1;
      t := access !t (modE x n)
    done;
  done;
  let (final,_) = sfc_step !s in
  (!t,final);;

let main n = 
  let (t,final) = test (if n == 0 then 100000 else n) 100 in
  Printf.printf "sum: %d, height: %d/%d, top: %d, final access: %d\n" (sum t) (maxheight t) (minheight t) (top t) final;;

main (int_of_string Sys.argv.(1));; 
