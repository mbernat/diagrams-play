{-

This module exports structures and functions relevant for representing
a dynamic graph (i.e. a graph changing in time) as a stream of primitive
graph operations such as adding nodes and removing edges.

The operation metadata only carry timestamps and labels for now.

-}

module GraphStream
    ( GraphOp(..)
    , GraphStream
    )
where

import Data.Text


type GraphStream = [GraphOp]

type NodeLabel = Text
type EdgeLabel = Text
type Timestamp = Float
type NodeId = Text

data GraphOp
    = AddNode NodeId Timestamp NodeLabel
    | RemoveNode NodeId
    | AddEdge (NodeId, NodeId) Timestamp EdgeLabel
    | RemoveEdge (NodeId, NodeId)
