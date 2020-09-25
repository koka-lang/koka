-- Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.hs
-- Modified to be strict in the Tree fields
import System.Environment

data Color =
  Red | Black

data Tree α β =
  Leaf
  | Node !Color !(Tree α β) !α !β !(Tree α β)

fold :: (α -> β  -> σ  -> σ) -> Tree α β -> σ  -> σ
fold _ Leaf b               = b
fold f (Node _ l k v r)   b = fold f r (f k v (fold f l b))

balanceL :: Tree α β -> α -> β -> Tree α β -> Tree α β
balanceL l k v r
  = case l of
      Leaf -> Leaf
      Node _  (Node Red lx kx vx rx) ky vy ry
        -> Node Red (Node Black lx kx vx rx) ky vy (Node Black ry k v r)
      Node _ ly ky vy (Node Red lx kx vx rx)
        -> Node Red (Node Black ly ky vy lx) kx vx (Node Black rx k v r)
      Node _ lx kx vx rx
        -> Node Black (Node Red lx kx vx rx) k v r
    
balanceR :: Tree α β -> α -> β -> Tree α β -> Tree α β
balanceR l k v r 
  = case r of
      Leaf -> Leaf
      Node _ (Node Red lx kx vx rx) ky vy ry
        -> Node Red (Node Black l k v lx) kx vx (Node Black rx ky vy ry)
      Node _ lx kx vx (Node Red ly ky vy ry)
        -> Node Red (Node Black l k v lx) kx vx (Node Black ly ky vy ry)
      Node _ lx kx vx rx
        -> Node Black l k v (Node Red lx kx vx rx)

is_red :: Tree α β -> Bool
is_red (Node Red _ _ _ _) = True
is_red _                  = False

lt x y = x < y

ins :: Ord α => Tree α β -> α -> β -> Tree α β
ins Leaf                 kx vx = Node Red Leaf kx vx Leaf
ins (Node Red a ky vy b) kx vx =
   (if lt kx ky then Node Red (ins a kx vx) ky vy b
    else if lt ky kx then Node Red a ky vy (ins b kx vx)
    else Node Red a ky vy (ins b kx vx))
ins (Node Black a ky vy b) kx vx =
    if lt kx ky then
      (if is_red a then balanceL (ins a kx vx) ky vy b
       else Node Black (ins a kx vx) ky vy b)
    else if lt ky kx then
      (if is_red b then balanceR a ky vy (ins b kx vx)
       else Node Black a ky vy (ins b kx vx))
    else Node Black a kx vx b

set_black :: Tree α β -> Tree α β
set_black (Node _ l k v r) = Node Black l k v r
set_black e                = e

insert t k v =
  if is_red t then set_black (ins t k v)
  else ins t k v


fuse l r 
 = case(l,r) of
    (Leaf,_) -> r
    (_,Leaf) -> l
    (Node Red lx kx vx rx, Node Red ly ky vy ry) -> case (fuse rx ly) of
      Node Red lz kz vz rz -> Node Red (Node Red lx kx vx lz) kz vz (Node Red rz ky vy ry)
      z -> Node Red lx kx vx (Node Red z ky vy ry)
    (Node Black lx kx vx rx, Node Black ly ky vy ry) -> case (fuse rx ly) of
      Node Red lz kz vz rz -> Node Red (Node Black lx kx vx lz) kz vz (Node Black rz ky vy ry)
      z -> del_bal_left lx kx vx (Node Black z ky vy ry)
    (_,Node Red ly ky vy ry) -> Node Red (fuse l ly) ky vy ry
    (Node Red lx kx vx rx,_) -> Node Red lx kx vx (fuse rx r)
  
balance l k v r
  = case (l,r) of
    (Node Red lx kx vx rx, Node Red ly ky vy ry)
      -> Node Red (Node Black lx kx vx rx) k v (Node Black ly ky vy ry)
    {-
    (Node Black _ _ _ _, _) -> balanceR l k v r
    _                       -> balanceL l k v r
    -}
    (Node Red lx kx vx (Node Red ly ky vy ry), _)
      -> Node Red (Node Black lx kx vx ly) ky vy (Node Black ry k v r)
    (_,Node Red lx kx vx (Node Red ly ky vy ry))
      -> Node Red (Node Black l k v lx) kx vx (Node Black ly ky vy ry)
    (_,Node Red (Node Red ly ky vy ry) kx vx rx)
      -> Node Red (Node Black l k v ly) ky vy (Node Black ry kx vx rx)
    _ -> Node Black l k v r
  
subl (Node Black l k v r) = Node Red l k v r
  
del_bal_left l k v r
 = case (l,r) of
    (Node Red ly ky vy ry, _)  -> Node Red (Node Black ly ky vy ry) k v r
    (_,Node Black lx kx vx rx) -> balance l k v (Node Red lx kx vx rx)
    (_,Node Red (Node Black ly ky vy ry) kx vx rx)
      -> Node Red (Node Black l k v ly) ky vy (balance ry kx vx (subl rx))

del_bal_right l k v r
  = case (l,r) of
    (_, Node Red ly ky vy ry)   -> Node Red l k v (Node Black ly ky vy ry)
    (Node Black lx kx vx rx, _) -> balance (Node Red lx kx vx rx) k v r
    (Node Red lx kx vx (Node Black ly ky vy ry), _) 
      -> Node Red (balance (subl lx) kx vx ly) ky vy (Node Black ry k v r)

is_bnode (Node Black _ _ _ _) = True
is_bnode _ = False

delete t key
  = case t of
      Node _ l k v r -> case compare key k of
        LT -> if (is_bnode l) then del_bal_left (delete l key) k v r
                              else Node Red (delete l key) k v r
        GT -> if (is_bnode r) then del_bal_right l k v (delete r key)
                              else Node Red l k v (delete r key)
        EQ -> fuse l r
      Leaf -> Leaf



type Map = Tree Int Bool

mk_Map_aux :: Int -> Int -> Map -> Map
mk_Map_aux total 0 t = t
mk_Map_aux total n t 
  = let n1 = n-1 
        t1 = insert t n1 (n1 `mod` 10 == 0)
        t2 = if (n1 `mod` 4 == 0) then delete t1 (n1 + (total - n1) `div` 4) else t1
    in mk_Map_aux total n1 t2

mk_Map n = mk_Map_aux n n Leaf

main = do
  -- [arg] <- getArgs
  -- let n :: Int = read arg
  let n = 4200000
  let m = mk_Map n
  let v = fold (\_ v r -> if v then r + 1 else r) m 0
  print v
