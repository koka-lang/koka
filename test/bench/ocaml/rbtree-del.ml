(* Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.ml *)

type color =
| Red
| Black;;

type node =
| Leaf
| Node of color * node * int * bool * node;;

let balance_left n kv vv t =
match n with
| Node (c, Node (Red, l, kx, vx, r1), ky, vy, r2) -> Node (Red, Node (Black, l, kx, vx, r1), ky, vy, Node (Black, r2, kv, vv, t))
| Node (c, l1, ky, vy, Node (Red, l2, kx, vx, r)) -> Node (Red, Node (Black, l1, ky, vy, l2), kx, vx, Node (Black, r, kv, vv, t))
| Node (c, l,  ky, vy, r)                         -> Node (Black, Node (Red, l, ky, vy, r), kv, vv, t)
| n -> Leaf;;

let balance_right t kv vv n =
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
  else if ky == kx then Node (Red, a, kx, vx, b)
  else Node (Red, a, ky, vy, ins b kx vx)
| Node (Black, a, ky, vy, b) ->
  if kx < ky then
    (if is_red a then balance_left (ins a kx vx) ky vy b 
      else Node (Black, (ins a kx vx), ky, vy, b))
  else if kx == ky then Node (Black, a, kx, vx, b)
  else if is_red b then balance_right a ky vy (ins b kx vx)
       else Node (Black, a, ky, vy, (ins b kx vx));;

let set_black n =
match n with
| Node (_, l, k, v, r) -> Node (Black, l, k, v, r)
| e                    -> e;;

let insert t k v =
if is_red t then set_black (ins t k v)
else ins t k v;;

let balance l k v r =
match (l,r) with
| (Node(Red,lx,kx,vx,rx), Node(Red,ly,ky,vy,ry)) -> Node (Red, Node (Black,lx,kx,vx,rx),k,v,Node (Black,ly,ky,vy,ry))
| (Node(Red,lx,kx,vx,Node(Red,ly,ky,vy,ry)), _) -> Node (Red, Node (Black,lx,kx,vx,ly),ky,vy,Node (Black,ry,k,v,r))
| (_,Node (Red,lx,kx,vx, Node (Red,ly,ky,vy,ry))) -> Node (Red, Node (Black,l,k,v,lx), kx, vx, Node (Black,ly, ky, vy, ry))
| (_,Node (Red,Node (Red,ly,ky,vy,ry),kx,vx,rx)) -> Node (Red, Node (Black, l, k, v, ly), ky, vy, Node (Black, ry, kx, vx, rx))
| (_,_) -> Node (Black,l,k,v,r)


let subl t =
match t with 
| Node(Black,l,k,v,r) -> Node (Red,l,k,v,r) ;;

let del_bal_left l k v r =
match l with
| Node(Red,ly,ky,vy,ry) -> Node (Red, Node (Black,ly,ky,vy,ry),k,v,r)
| _ -> (match r with
        | Node(Black,lx,kx,vx,rx) -> balance l k v (Node (Red,lx,kx,vx,rx))
        | Node(Red, Node (Black,ly,ky,vy,ry),kx,vx,rx) -> Node (Red, Node (Black,l,k,v,ly) ,ky, vy, balance ry kx vx (subl rx))
        (*| _ -> Node(Black,l,k,v,r) *)
       );;

(*
match (l,r) with
| (Node(Red,ly,ky,vy,ry),_)   -> Node (Red, Node (Black,ly,ky,vy,ry),k,v,r)
| (_,Node(Black,lx,kx,vx,rx)) -> balance l k v (Node (Red,lx,kx,vx,rx))
| (_,Node(Red,Node(Black,ly,ky,vy,ry),kx,vx,rx)) -> Node (Red, Node (Black,l,k,v,ly) ,ky, vy, balance ry kx vx (subl rx) )  ;;
*)

let del_bal_right l k v r = 
match r with 
| Node(Red,ly,ky,vy,ry) -> Node(Red,l,k,v,Node (Black,ly,ky,vy,ry))
| _ -> (match l with
        | Node(Black,lx,kx,vx,rx) -> balance (Node (Red,lx,kx,vx,rx)) k v r         
        | Node(Red,lx,kx,vx,Node(Black,ly,ky,vy,ry)) -> Node (Red, balance (subl lx) kx vx ly, ky, vy, Node (Black,ry,k,v,r))
        (*| _ -> Node(Black,l,k,v,r)*)
        );;

(*
match (l,r) with 
| (_,Node(Red,ly,ky,vy,ry))   -> Node(Red,l,k,v,Node (Black,ly,ky,vy,ry))
| (Node(Black,lx,kx,vx,rx),_) -> balance (Node (Red,lx,kx,vx,rx)) k v r         
| (Node(Red,lx,kx,vx,Node(Black,ly,ky,vy,ry)),_) -> Node (Red, balance (subl lx) kx vx ly, ky, vy, Node (Black,ry,k,v,r))  ;;
*)

let rec fuse l r =
match (l,r) with
| (Leaf,_) -> r
| (_,Leaf) -> l
| (Node(Black,lx,kx,vx,rx),Node(Black,ly,ky,vy,ry)) -> (match fuse rx ly with 
    | Node(Red,lz,kz,vz,rz) -> Node(Red,Node(Black,lx,kx,vx,lz),kz,vz,Node(Black,rz,ky,vy,ry))
    | z -> del_bal_left lx kx vx (Node (Black,z,ky,vy,ry)))
| (Node(Red,lx,kx,vx,rx),Node(Red,ly,ky,vy,ry)) -> (match fuse rx ly with
    | Node(Red,lz,kz,vz,rz) -> Node (Red, Node (Red,lx,kx,vx,lz),kz,vz,Node(Red,rz,ky,vy,ry))
    | z -> Node(Red,lx,kx,vx,Node(Red,z,ky,vy,ry)))
| (_,Node(Red,ly,ky,vy,ry)) -> Node (Red, fuse l ly, ky, vy, ry)
| (Node(Red,lx,kx,vx,rx),_) -> Node (Red, lx, kx, vx, fuse rx r ) ;;

let is_bnode t =
match t with
| Node (Black,_,_,_,_) -> true
| _ -> false;;
  

let rec delete t key =
match t with 
| Node(_,l,k,v,r) 
   -> if (k < key) then (if (is_bnode l) then del_bal_left (delete l key) k v r
                                         else Node(Red,delete l key,k,v,r))
      else if (k > key) then (if (is_bnode r) then del_bal_right l k v (delete r key)
                                              else Node(Red,l,k,v,delete r key))
      else fuse l r
| Leaf -> Leaf ;;


let rec fold f n d =
match n with
| Leaf -> d
| Node(_, l, k, v, r) -> fold f r (f k v (fold f l d));;

let rec mk_map_aux total n t =
if n = 0 then t
else let n1 = n-1 in
     let t1 = insert t n1 (n1 mod 10 == 0) in
     let t2 = if (n1 mod 4 == 0) then delete t1 (n1 + (total - n1)/4) else t1 in
     mk_map_aux total n1 t2;;

let mk_map n = mk_map_aux n n Leaf;;

let main n =
let m = mk_map n in
let v = fold (fun k v r -> if v then r + 1 else r) m 0 in
Printf.printf "%8d\n" v;
v;;

(* main (int_of_string Sys.argv.(1));; *)
main 4200000;;
