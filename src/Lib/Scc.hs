------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
  Compute the /strongly connected components/ of a directed graph.
  The implementation is based on the following article:

  * David King and John Launchbury, /Lazy Depth-First Search and Linear Graph Algorithms in Haskell/,
    ACM Principles of Programming Languages, San Francisco, 1995.

  In contrast to their description, this module doesn't use lazy state
  threads but is instead purely functional -- using the "Map" and "Set" module.
  This means that the complexity of 'scc' is /O(n*log n)/ instead of /O(n)/ but
  due to the hidden constant factor, this implementation performs very well in practice.
-}
-----------------------------------------------------------------------------
module Lib.Scc ( scc ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
-- just for testing
import Debug.QuickCheck
import List(nub,sort)
-}

{--------------------------------------------------------------------
  Graph
--------------------------------------------------------------------}
-- | A @Graph v@ is a directed graph with nodes @v@.
newtype Graph v = Graph (Map.Map v [v])

-- | An @Edge v@ is a pair @(x,y)@ that represents an arrow from
-- node @x@ to node @y@.
type Edge v     = (v,v)
type Node v     = (v,[v])

{--------------------------------------------------------------------
  Conversion
--------------------------------------------------------------------}
nodes :: Graph v -> [Node v]
nodes (Graph g)
  = Map.toList g

graph :: Ord v => [Node v] -> Graph v
graph es
  = Graph (Map.fromListWith (++) es)

{--------------------------------------------------------------------
  Graph functions
--------------------------------------------------------------------}
edges :: Graph v -> [Edge v]
edges g
  = [(v,w) | (v,vs) <- nodes g, w <- vs]

vertices :: Graph v -> [v]
vertices g
  = [v | (v,vs) <- nodes g]

successors :: Ord v => v -> Graph v -> [v]
successors v (Graph g)
  = Map.findWithDefault [] v g

transpose :: Ord v => Graph v -> Graph v
transpose g@(Graph m)
  = Graph (foldr add empty (edges g))
  where
    empty       = Map.map (const []) m
    add (v,w) m = Map.adjust (v:) w m


{--------------------------------------------------------------------
  Depth first search and forests
--------------------------------------------------------------------}
data Tree v   = Node v (Forest v)
type Forest v = [Tree v]

dff :: Ord v => Graph v -> Forest v
dff g
  = dfs g (vertices g)

dfs :: Ord v => Graph v -> [v] -> Forest v
dfs g vs
  = prune (map (tree g) vs)

tree :: Ord v => Graph v -> v -> Tree v
tree g v
  = Node v (map (tree g) (successors v g))

prune :: Ord v => Forest v -> Forest v
prune fs
  = snd (chop Set.empty  fs)
  where
    chop ms []  = (ms,[])
    chop ms (Node v vs:fs)
      | visited   = chop ms fs
      | otherwise = let ms0       = Set.insert v ms
                        (ms1,vs') = chop ms0 vs
                        (ms2,fs') = chop ms1 fs
                    in (ms2,Node v vs':fs')
      where
        visited   = Set.member v ms

{--------------------------------------------------------------------
  Orderings
--------------------------------------------------------------------}
preorder :: Ord v => Graph v -> [v]
preorder g
  = preorderF (dff g)

preorderF fs
  = concatMap preorderT fs

preorderT (Node v fs)
  = v:preorderF fs

postorder :: Ord v => Graph v -> [v]
postorder g
  = postorderF (dff g)

postorderT t
  = postorderF [t]

postorderF ts
  = postorderF' ts []
  where
    -- efficient concatenation by passing the tail around.
    postorderF' [] tl          = tl
    postorderF' (t:ts) tl      = postorderT' t (postorderF' ts tl)
    postorderT' (Node v fs) tl = postorderF' fs (v:tl)


{--------------------------------------------------------------------
  Strongly connected components
--------------------------------------------------------------------}

{- |
 Compute the strongly connected components of a graph. The algorithm
 is tailored toward the needs of compiler writers that need to compute
 recursive binding groups (for example, the original order is preserved
 as much as possible).

 The expression (@scc xs@) computes the strongly connectected components
 of graph @xs@. A graph is a list of nodes @(v,ws)@ where @v@ is the node
 label and @ws@ a list of nodes where @v@ points to, ie. there is an
 arrow\/dependency from @v@ to each node in @ws@. Here is an example
 of @scc@:

>  Scc\> scc [(0,[1]),(1,[1,2,3]),(2,[1]),(3,[]),(4,[])]
>  [[3],[1,2],[0],[4]]

 In an expression @(scc xs)@, the graph @xs@ should contain an entry for
 every node in the graph, ie:

>  all (`elem` nodes) targets
>  where nodes   = map fst xs
>        targets = concat (map snd xs)

 Furthermore, the returned components consist exactly of the original nodes:

>  sort (concat (scc xs)) == sort (map fst xs)

 The connected components are sorted by dependency, ie. there are
 no arrows\/dependencies from left-to-right. Furthermore, the original order
 is preserved as much as possible.
-}
scc :: Ord v => [(v,[v])] -> [[v]]
scc nodes
  = sccG (graph nodes)

sccG :: Ord v => Graph v -> [[v]]
sccG g
  = map preorderT (sccF g)

sccF :: Ord v => Graph v -> Forest v
sccF g
  = reverse (dfs (transpose g) (topsort g))

topsort g
  = reverse (postorder g)

{--------------------------------------------------------------------
  Reachable and path
--------------------------------------------------------------------}
reachable v g
  = preorderF (dfs g [v])

path v w g
  = elem w (reachable v g)


{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show v => Show (Graph v) where
  showsPrec d (Graph m) = shows m

instance Show v => Show (Tree v) where
  showsPrec d (Node v []) = shows v
  showsPrec d (Node v fs) = shows v . showList fs


{--------------------------------------------------------------------
  Quick Test
--------------------------------------------------------------------}
tgraph0 :: Graph Int
tgraph0 = graph
          [(0,[1])
          ,(1,[2,1,3])
          ,(2,[1])
          ,(3,[])
          ]

tgraph1 = graph
          [  ('a',"jg")
          ,  ('b',"ia")
          ,  ('c',"he")
          ,  ('d',"")
          ,  ('e',"jhd")
          ,  ('f',"i")
          ,  ('g',"fb")
          ,  ('h',"")
          ]

{-
{--------------------------------------------------------------------
  Quickcheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary Graph's
--------------------------------------------------------------------}
instance (Ord v,Arbitrary v) => Arbitrary (Graph v) where
  arbitrary   = sized arbgraph


arbgraph :: (Ord v,Arbitrary v) => Int -> Gen (Graph v)
arbgraph n
  = do nodes <- arbitrary
       g     <- mapM (targets nodes) nodes
       return (graph g)
  where
    targets nodes v
      = do sz <- choose (0,length nodes-1)
           ts <- mapM (target nodes) [1..sz]
           return (v,ts)

    target nodes _
      = do idx <- choose (0,length nodes-1)
           return (nodes!!idx)

{--------------------------------------------------------------------
  Properties
--------------------------------------------------------------------}
prop_ValidGraph :: Graph Int -> Bool
prop_ValidGraph g
  = all (`elem` srcs) targets
  where
    srcs    = map fst (nodes g)
    targets = concatMap snd (nodes g)

-- all scc nodes are in the original graph and the other way around
prop_SccComplete :: Graph Int -> Bool
prop_SccComplete g
  = sort (concat (sccG g)) == sort (vertices g)

-- all scc nodes have only backward dependencies
prop_SccForward :: Graph Int -> Bool
prop_SccForward g
  = all noforwards (zip prevs ss)
  where
    ss      = sccG g
    prevs   = scanl1 (++) ss

    noforwards (prev,xs)
      = all (noforward prev) xs

    noforward prev x
      = all (`elem` prev) (successors x g)

-- all strongly connected components refer to each other
prop_SccConnected :: Graph Int -> Bool
prop_SccConnected g
  = all connected (sccG g)
  where
    connected xs
      = all (paths xs) xs

    paths xs x
      = all (\y -> path x y g) xs

-}
