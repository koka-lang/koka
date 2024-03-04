open Int32

type ztree =
| Leaf
| Node of int * ztree * int * ztree;;

type zipper =
| Done
| NodeR of int * ztree * int * zipper
| NodeL of int * zipper * int * ztree;;

let rec ctz x acc =
  if ((logand x one) = one) 
    then acc
    else ctz (shift_right_logical x 1) (acc + 1);;

let count_trailing_zeros x =
  if (x = zero) then 32 else ctz x 0;;

let rank_of1 k =
  let x0 = of_int k in
  let x1 = mul (logxor x0 (shift_right_logical x0 15)) (of_int 0x2c1b3c6d) in
  let x2 = mul (logxor x1 (shift_right_logical x1 12)) (of_int 0x297a2d39) in
  let x  = logxor x2 (shift_right_logical x2 15) in
  begin
    let cnt = count_trailing_zeros x in
    cnt
  end;;

let rank_of k =
  let x0 = of_int (k + 1) in
  let x1 = mul (logxor x0 (shift_right_logical x0 16)) (of_int 0x297a2d39) in
  let x  = logxor x1 (shift_right_logical x1 16) in
  begin
    let cnt = count_trailing_zeros x in
    cnt
  end;;  

let is_higher_rank r1 k1 r2 k2 =
  (r1 > r2) || (r1 == r2 && k1 < k2);;

let rec rebuild z t =
  match z with
  | NodeR(rnk,l,x,up) -> rebuild up (Node(rnk,l,x,t))
  | NodeL(rnk,up,x,r) -> rebuild up (Node(rnk,t,x,r))
  | Done              -> t;;

let rec unzip t k zs zb =
  match t with
  | Node(rnk,l,x,r) -> if (x < k) then unzip r k (NodeR(rnk,l,x,zs)) zb
                       else if (x > k) then unzip l k zs (NodeL(rnk,zb,x,r))
                       else (rebuild zs l, rebuild zb r)
  | Leaf -> (rebuild zs Leaf, rebuild zb Leaf);;

let rec find t rank k z =
  match t with
  | Node (rnk,l,x,r) when is_higher_rank rnk x rank k
      -> if (x < k) then find r rank k (NodeR (rnk,l,x,z))
                    else find l rank k (NodeL (rnk,z,x,r))
  | Node (_,_,x,_) when x == k -> rebuild z t
  | _ -> let (s,b) = unzip t k Done Done 
         in rebuild z (Node (rank,s,k,b));;

let access t k = find t (rank_of k) k Done;;  


(* helpers *)

let rec sumacc t acc = 
  match t with
  | Node (rnk,l,x,r) -> sumacc r (sumacc l (acc+x))
  | Leaf             -> acc;;

let sum t = sumacc t 0;;

let imin x y = if (x <= y) then x else y;;
let imax x y = if (x >= y) then x else y;;

let rec minheight t = 
  match t with
  | Node (rnk,l,x,r) -> 1 + imin (minheight l) (minheight r)
  | Leaf             -> 0;;

let rec maxheight t = 
  match t with
  | Node (rnk,l,x,r) -> 1 + imax (maxheight l) (maxheight r)
  | Leaf             -> 0;;

let top t = 
  match t with
  | Node (rnk,l,x,r) -> x
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
