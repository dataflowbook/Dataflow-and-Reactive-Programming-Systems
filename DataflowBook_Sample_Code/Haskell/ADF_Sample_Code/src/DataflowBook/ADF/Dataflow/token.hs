{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataflowBook.ADF.Dataflow.Token

(T
,mk,new
,getVal ,getPort ,getNode ,setNodeAndPort
) where

import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.Val as Val
import qualified DataflowBook.ADF.Dataflow.Local as Local

import qualified Data.List as Lst

data T t = Token Id.NodeId Id.PortId (Val.T t)
    deriving (Eq,Show)


mk :: Id.NodeId -> Id.PortId -> Val.T t -> T t
mk node port val = Token node port val

new :: (String, String) -> t -> T t
new (nodeid,portid) val = Token (Id.mkNodeId nodeid) (Id.mkPortId portid) (Val.mk val)

-----------
-- Get/Set
-----------

getVal :: T t -> Val.T t
getVal (Token _ _ val) = val

getPort :: T t -> Id.PortId
getPort (Token _ id _) = id

getNode :: T t -> Id.NodeId
getNode (Token id _ _) = id

setNodeAndPort :: Id.NodeId -> Id.PortId -> T t -> T t
setNodeAndPort nid pid (Token _ _ val) = Token nid pid val


