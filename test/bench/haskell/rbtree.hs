-- Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/rbmap.hs
-- Modified to be strict in the Tree fields
import System.Environment

data Color = Red | Black

data Tree k v
  = Leaf
  | Node !Color !(Tree k v) !k !v !(Tree k v)

fold :: (a -> b  -> c  -> c) -> Tree a b -> c  -> c
fold _ Leaf b               = b
fold f (Node _ l k v r)   b = fold f r (f k v (fold f l b))

balance1 :: Tree a b -> Tree a b -> Tree a b
balance1 (Node _ _ kv vv t) (Node _ (Node Red l kx vx r₁) ky vy r₂) = Node Red (Node Black l kx vx r₁) ky vy (Node Black r₂ kv vv t)
balance1 (Node _ _ kv vv t) (Node _ l₁ ky vy (Node Red l₂ kx vx r)) = Node Red (Node Black l₁ ky vy l₂) kx vx (Node Black r kv vv t)
balance1 (Node _ _ kv vv t) (Node _ l  ky vy r)                     = Node Black (Node Red l ky vy r) kv vv t
balance1 _                                                        _ = Leaf

balance2 :: Tree a b -> Tree a b -> Tree a b
balance2 (Node _ t kv vv _) (Node _ (Node Red l kx₁ vx₁ r₁) ky vy r₂)  = Node Red (Node Black t kv vv l) kx₁ vx₁ (Node Black r₁ ky vy r₂)
balance2 (Node _ t kv vv _) (Node _ l₁ ky vy (Node Red l₂ kx₂ vx₂ r₂)) = Node Red (Node Black t kv vv l₁) ky vy (Node Black l₂ kx₂ vx₂ r₂)
balance2 (Node _ t kv vv _) (Node _ l ky vy r)                         = Node Black t kv vv (Node Red l ky vy r)
balance2 _                                                        _    = Leaf

is_red :: Tree a b -> Bool
is_red (Node Red _ _ _ _) = True
is_red _                  = False

ins :: Ord a => Tree a b -> a -> b -> Tree a b
ins Leaf                 kx vx = Node Red Leaf kx vx Leaf
ins (Node Red l ky vy r) kx vx =
   (if kx < ky then Node Red (ins l kx vx) ky vy r
    else if ky < kx then Node Red l ky vy (ins r kx vx)
    else Node Red l kx vx r) -- Node Red l ky vy (ins r kx vx))
ins (Node Black l ky vy r) kx vx =
    if kx < ky then
      (if is_red l then balance1 (Node Black Leaf ky vy r) (ins l kx vx)
       else Node Black (ins l kx vx) ky vy r)
    else if ky < kx then
      (if is_red r then balance2 (Node Black l ky vy Leaf) (ins r kx vx)
       else Node Black l ky vy (ins r kx vx))
    else Node Black l kx vx r

set_black :: Tree a b -> Tree a b
set_black (Node _ l k v r) = Node Black l k v r
set_black e                = e

insert t k v =
  if is_red t then set_black (ins t k v)
  else ins t k v

type Map = Tree Int Bool

mk_Map_aux :: Int -> Map -> Map
mk_Map_aux 0 m = m
mk_Map_aux n m = let n' = n-1 in mk_Map_aux n' (insert m n' (n' `mod` 10 == 0))

mk_Map n = mk_Map_aux n Leaf

main = do
  -- [arg] <- getArgs
  -- let n :: Int = read arg
  let n = 4200000
  let m = mk_Map n
  let v = fold (\_ v r -> if v then r + 1 else r) m 0
  print v
