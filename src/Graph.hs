{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-

This module exports a simple graph data structure.
It's intended as a test-bed for folding of graph stream operations.

-}


module Graph
    ( Edge(..)
    , Graph(..)
    , Node(..)
    , applyGraphOp
    , empty
    )
where

import GraphStream


{-

TODO think harder about graph and graph stream API
TODO consider using focused streams (a la zippers)
TODO consider representing the streams continuously (as step functions)

-}

data Node = Node
    { nodeId :: NodeId
    , nodeLabel :: NodeLabel
    }
  deriving (Show)

data Edge = Edge
    { edgeId :: (NodeId, NodeId)
    , edgeLabel :: EdgeLabel
    }
  deriving (Show)

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    }
  deriving (Show)

empty :: Graph
empty = Graph [] []

-- | Function suitable for folding the stream of graph operations.
applyGraphOp :: Graph -> GraphOp -> Graph
applyGraphOp g = \case
    AddNode nodeId label -> addNode nodeId label g
    RemoveNode nodeId -> removeNode nodeId g
    AddEdge e label -> addEdge e label g
    RemoveEdge e -> removeEdge e g

-- TODO what to do when a node with the given ID already exists?

-- | Add a node with the given ID and label.
addNode :: NodeId -> NodeLabel -> Graph -> Graph
addNode nodeId label Graph{..} = Graph
    { nodes = node : nodes
    , edges = edges
    }
  where
    node = Node
        { nodeId = nodeId
        , nodeLabel = label
        }
-- | Remove 0 or more nodes with the given ID.
removeNode :: NodeId -> Graph -> Graph
removeNode nId Graph{..} = Graph
    { nodes = filter (\n -> nodeId n /= nId) nodes
    , edges = edges
    }

-- TODO do we allow multiple edges between the same nodes?
-- TODO what should we do when the nodes are not in the graph?

-- | Add a labelled edge between the nodes specified by IDs.
addEdge :: (NodeId, NodeId) -> EdgeLabel -> Graph -> Graph
addEdge edgeId label Graph{..} = Graph
    { nodes = nodes
    , edges = edge : edges
    }
  where
    edge = Edge
        { edgeId = edgeId
        , edgeLabel = label
        }

-- | Remove 0 or more edges between nodes specified by IDs.
removeEdge :: (NodeId, NodeId) -> Graph -> Graph
removeEdge eId Graph{..} = Graph
    { nodes = nodes
    , edges = filter (\e -> edgeId e /= eId) edges
    }
