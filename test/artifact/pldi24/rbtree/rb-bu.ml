(* Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.ml *)
(* This is a bottom up implementation following Lorenzen and Leijen's "frame limited reuse" implementation (ICFP'22) *)

open Int32;;

type color =
| Red
| Black;;

type tree =
| Leaf
| Node of color * tree * int * int * tree;;

type zipper =
| Done
| NodeR of color * tree * int * int * zipper
| NodeL of color * zipper * int * int * tree;;


let is_red t =
  match t with
  | Node (Red, _, _, _, _) -> true
  | _ -> false;;

let set_black n =
  match n with
  | Node (_, l, k, v, r) -> Node (Black, l, k, v, r)
  | e                    -> e;;

let rec rebuild z t =
  match z with
  | NodeR(c,l,k,v,up) -> rebuild up (Node(c,l,k,v,t))
  | NodeL(c,up,k,v,r) -> rebuild up (Node(c,t,k,v,r))
  | Done              -> t;;
 
let rec balance z l k v r =  
  match z with
  | NodeR(Black,l1,k1,v1,z1) -> rebuild z1 (Node(Black,l1,k1,v1,Node(Red,l,k,v,r)))
  | NodeL(Black,z1,k1,v1,r1) -> rebuild z1 (Node(Black,Node(Red,l,k,v,r),k1,v1,r1))
  (* red red violation *)
  | NodeR(Red,l1,k1,v1,z1) -> begin match z1 with
    | NodeR(_,l2,k2,v2,z2) -> balance z2 (Node(Black,l2,k2,v2,l1)) k1 v1 (Node(Black,l,k,v,r))
    | NodeL(_,z2,k2,v2,r2) -> balance z2 (Node(Black,l1,k1,v1,l)) k v (Node(Black,r,k2,v2,r2))
    | Done                 -> Node(Black,l1,k1,v1,Node(Red,l,k,v,r))
    end
  | NodeL(Red,z1,k1,v1,r1) -> begin match z1 with
    | NodeR(_,l2,k2,v2,z2) -> balance z2 (Node(Black,l2,k2,v2,l)) k v (Node(Black,r,k1,v1,r1))
    | NodeL(_,z2,k2,v2,r2) -> balance z2 (Node(Black,l,k,v,r)) k1 v1 (Node(Black,r1,k2,v2,r2))
    | Done                 -> Node(Black,Node(Red,l,k,v,r),k1,v1,r1)
    end
  | Done -> Node(Black,l,k,v,r);;
  
let rec find t k v z =
  match t with
  | Node(c,l,kx,vx,r) -> if (kx < k) then find r k v (NodeR(c,l,kx,vx,z))
                          else if (kx > k) then find l k v (NodeL(c,z,kx,vx,r))
                                           else rebuild z (Node(c,l,k,v,r))
  | Leaf -> balance z Leaf k v Leaf;;

let access t k =
  find t k k Done
  (* if is_red t then set_black (find t k k Done)
     else ins t k k;; *)

let rec fold f n d =
match n with
| Leaf -> d
| Node(_, l, k, v, r) -> fold f r (f k v (fold f l d));;


(* helpers *)

let rec sumacc t acc = 
  match t with
  | Node (c,l,k,x,r) -> sumacc r (sumacc l (acc+x))
  | Leaf           -> acc;;

let sum t = sumacc t 0;;


let imin x y = if (x <= y) then x else y;;
let imax x y = if (x >= y) then x else y;;

let rec minheight t = 
  match t with
  | Node (_,l,_,_,r) -> 1 + imin (minheight l) (minheight r)
  | Leaf             -> 0;;

let rec maxheight t = 
  match t with
  | Node (_,l,_,_,r) -> 1 + imax (maxheight l) (maxheight r)
  | Leaf             -> 0;;

let top t = 
  match t with
  | Node (_,l,x,_,r) -> x
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
