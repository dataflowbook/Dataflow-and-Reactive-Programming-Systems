{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataflowBook.ADF.Dataflow.Node
(T
,mk,new
,getPorts
,getFn
,getId
,getPatterns
,getOutputPortIds
,getPortId
,findNode
) where

import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.FirePattern as FirePattern
import qualified DataflowBook.ADF.Dataflow.Port as Port
import qualified DataflowBook.ADF.Dataflow.Def as Def
import qualified DataflowBook.ADF.Dataflow.Local as Local

import qualified Data.List as Lst


data T a
    = Node Id.NodeId (Def.T a)
    deriving Show

mk :: Id.NodeId -> (Def.T t) -> T t
mk = Node

new :: String -> Def.T t -> T t
new nodeid def = Node (Id.mkNodeId nodeid) def

-----------
-- Get/Set
-----------

getPorts :: T t -> [Port.T]
getPorts (Node _ def) = Def.getPorts def

getId :: T t -> Id.NodeId
getId (Node id _) = id

getFn :: T t -> ([Local.T t] -> [Local.T t])
getFn (Node _ def) = Def.getFn def

getPortId :: Port.T -> Id.PortId
getPortId = Port.getId

getOutputPortIds :: T t -> [Id.PortId]
getOutputPortIds node =
    (map getPortId . filter Port.isOutput) ports

    where
        ports = getPorts node

getInputPortIds :: T t -> [Id.PortId]
getInputPortIds node =
    (map getPortId . filter Port.isInput) ports

    where
        ports = getPorts node

getPatterns :: T t -> [[FirePattern.T]]
getPatterns (Node _ def) = Def.getFirePattern def

----------
-- Search
----------

findNode :: Id.NodeId -> [T t] -> Maybe (T t)
findNode id nodes =
    Lst.find (\n -> id == getId n) nodes


