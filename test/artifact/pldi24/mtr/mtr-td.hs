import Tree
import GHC.Exts

data Root = Root{ left :: Tree, key :: Key, right :: Tree }

access :: Tree -> Key -> Tree
access t k  =
    let (Root l x r) =  accessz t k
    in Node l x r


accessz :: Tree -> Key -> Root
accessz t k
  = case t of
      Leaf -> Root Leaf k Leaf
      Node l x r ->
        if   x < k then
            let (Root s f b) = accessz r k
            in Root (Node l x s) f b
        else if x > k then
            let (Root s f b) = accessz l k
            in Root s f (Node b x r)
        else Root l x r

main :: IO ()
main
  = benchMain access
