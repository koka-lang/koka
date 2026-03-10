(* Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.ml *)
(* This is actually recursive and not quite bottom-up *)

open Int32;;

type color =
| Red
| Black;;

type node =
| Leaf
| Node of color * node * int * int * node;;

let balance1 kv vv t n =
match n with
| Node (c, Node (Red, l, kx, vx, r1), ky, vy, r2) -> Node (Red, Node (Black, l, kx, vx, r1), ky, vy, Node (Black, r2, kv, vv, t))
| Node (c, l1, ky, vy, Node (Red, l2, kx, vx, r)) -> Node (Red, Node (Black, l1, ky, vy, l2), kx, vx, Node (Black, r, kv, vv, t))
| Node (c, l,  ky, vy, r)                         -> Node (Black, Node (Red, l, ky, vy, r), kv, vv, t)
| n -> Leaf;;

let balance2 t kv vv n =
match n with
| Node (_, Node (Red, l, kx1, vx1, r1), ky, vy, r2)  -> Node (Red, Node (Black, t, kv, vv, l), kx1, vx1, Node (Black, r1, ky, vy, r2))
| Node (_, l1, ky, vy, Node (Red, l2, kx2, vx2, r2)) -> Node (Red, Node (Black, t, kv, vv, l1), ky, vy, Node (Black, l2, kx2, vx2, r2))
| Node (_, l, ky, vy, r)                             -> Node (Black, t, kv, vv, Node (Red, l, ky, vy, r))
| n   -> Leaf;;

let is_red t =
match t with
| Node (Red, _, _, _, _) -> true
| _ -> false;;

let rec ins t kx vx =
match t with
| Leaf -> Node (Red, Leaf, kx, vx, Leaf)
| Node (Red, a, ky, vy, b) ->
  if kx < ky then Node (Red, ins a kx vx, ky, vy, b)
  else if ky = kx then Node (Red, a, kx, vx, b)
  else Node (Red, a, ky, vy, ins b kx vx)
| Node (Black, a, ky, vy, b) ->
  if kx < ky then
    (if is_red a then balance1 ky vy b (ins a kx vx)
      else Node (Black, (ins a kx vx), ky, vy, b))
  else if kx = ky then Node (Black, a, kx, vx, b)
  else if is_red b then balance2 a ky vy (ins b kx vx)
       else Node (Black, a, ky, vy, (ins b kx vx));;

let set_black n =
match n with
| Node (_, l, k, v, r) -> Node (Black, l, k, v, r)
| e                    -> e;;

let access t k =
if is_red t then set_black (ins t k k)
else ins t k k;;

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
  Printf.printf "sum %d, final access: %d\n" (sum t) final;;

main (int_of_string Sys.argv.(1));; 
