{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataflowBook.ADF.Dataflow.Id
(NodeId,PortId
,mkNodeId,mkPortId
) where

-- I used strings here simply because it is example code
-- and string make it easy to play with.
-- NEVER use string for identifiers
-- UUIDs are best, a unique int is second best

newtype NodeId = NodeId String  
    deriving (Eq,Show)
newtype PortId = PortId String
    deriving (Eq,Show)

mkNodeId = NodeId
mkPortId = PortId
