import Tree


data Zipper
  = NodeR { left :: !Tree, key :: !Key, up :: !Zipper }
  | NodeL { up :: !Zipper, key :: !Key, right :: !Tree }
  | Done


access :: Tree -> Key -> Tree
access t k  = accessz t k Done


accessz :: Tree -> Key -> Zipper -> Tree
accessz t k z
  = case t of
      Leaf -> mtr z Leaf k Leaf
      Node l x r ->
        if   x < k then accessz r k (NodeR l x z)
        else if x > k then accessz l k (NodeL z x r)
        else mtr z l x r

mtr :: Zipper -> Tree -> Key -> Tree -> Tree
mtr z l x r
  = case z of
      Done -> Node l x r
      NodeR zl zx zz -> mtr zz (Node zl zx l) x r
      NodeL zz zx zr -> mtr zz l x (Node r zx zr)

main :: IO ()
main
  = benchMain access
