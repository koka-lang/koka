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
      Leaf -> splay z Leaf k Leaf
      Node l x r ->
        if   x < k then accessz r k (NodeR l x z)
        else if x > k then accessz l k (NodeL z x r)
        else splay z l x r

splay :: Zipper -> Tree -> Key -> Tree -> Tree
splay z l x r
  = case z of
      Done -> Node l x r
      NodeR zl zx zz -> case zz of
        Done -> Node (Node zl zx l) x r
        NodeR zzl zzx zzz -> splay zzz (Node (Node zzl zzx zl) zx l) x r
        NodeL zzz zzx zzr -> splay zzz (Node zl zx l) x (Node r zzx zzr)
      NodeL zz zx zr  -> case zz of
        Done -> Node l x (Node r zx zr)
        NodeR zzl zzx zzz -> splay zzz (Node zzl zzx l) x (Node r zx zr)
        NodeL zzz zzx zzr -> splay zzz l x (Node r zx (Node zr zzx zzr))

main :: IO ()
main
  = benchMain access
