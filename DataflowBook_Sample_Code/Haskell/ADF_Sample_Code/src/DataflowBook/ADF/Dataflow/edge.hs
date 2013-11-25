{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataflowBook.ADF.Dataflow.Edge
(T
,new
,getSourceNode,getSourcePort
,getSinkNode,getSinkPort
) where

import qualified DataflowBook.ADF.Dataflow.Id as Id

data T = Edge Id.NodeId Id.PortId Id.NodeId Id.PortId
    deriving Show


-- | Constructor
new (n1,p1) (n2,p2) = Edge (Id.mkNodeId n1)(Id.mkPortId p1)(Id.mkNodeId n2)(Id.mkPortId p2)


getSourceNode (Edge a _ _ _) = a
getSourcePort (Edge _ a _ _) = a
getSinkNode   (Edge _ _ a _) = a
getSinkPort   (Edge _ _ _ a) = a
