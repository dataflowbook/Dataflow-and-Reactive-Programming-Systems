{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataflowBook.ADF.Dataflow.Id
(NodeId,PortId
,mkNodeId,mkPortId
) where

newtype NodeId = NodeId String
    deriving (Eq,Show)
newtype PortId = PortId String
    deriving (Eq,Show)

mkNodeId = NodeId
mkPortId = PortId
