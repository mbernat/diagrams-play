{-# LANGUAGE OverloadedStrings #-}

{-

This module exports structures and functions relevant for representing
a dynamic graph (i.e. a graph changing in time) as a stream of primitive
graph operations such as adding nodes and removing edges.

The operation metadata only carry timestamps and labels for now.

-}


module GraphStream
    ( EdgeLabel
    , GraphOp(..)
    , GraphStream
    , NodeId
    , NodeLabel
    , Timestamp
    , selectStream
    )
where

import Data.Text (Text)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()


type GraphStream = [(Timestamp, GraphOp)]

type NodeLabel = Text
type EdgeLabel = Text
type Timestamp = Double
type NodeId = Text

data GraphOp
    = AddNode NodeId NodeLabel
    | RemoveNode NodeId
    | AddEdge (NodeId, NodeId) EdgeLabel
    | RemoveEdge (NodeId, NodeId)

instance Arbitrary GraphOp where
    arbitrary = oneof
        [ AddNode <$> arbitrary <*> arbitrary
        , RemoveNode <$> arbitrary
        , AddEdge <$> arbitrary <*> arbitrary
        , RemoveEdge <$> arbitrary
        ]
{-

TODO think harder about the graph stream API
TODO consider using focused streams (a la zippers)
TODO consider representing the streams continuously (as step functions)

-}


-- TODO make this efficient.
-- | Select the part of the stream up to the given moment.
selectStream :: Timestamp -> GraphStream -> GraphStream
selectStream upto = filter (\(t, _) -> t <= upto)
