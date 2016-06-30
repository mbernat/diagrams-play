module DivisionGraph
    ( divisionGraph
    )
where

import Data.Graph.Inductive.PatriciaTree
import Diagrams.TwoD.GraphViz

divides :: Integer -> Integer -> Bool
divides x y = x `mod` y == 0

rel :: Integer -> Integer -> Bool
rel x y = x `divides` y || y `divides` x

edges :: Integer -> [(Integer, Integer, ())]
edges y = [(x, y, ()) | x <- [1..(y-1)], x `rel` y]

divisionGraph :: Integer -> Gr Integer ()
divisionGraph n = mkGraph [1..n] $ concatMap edges [1..n]

