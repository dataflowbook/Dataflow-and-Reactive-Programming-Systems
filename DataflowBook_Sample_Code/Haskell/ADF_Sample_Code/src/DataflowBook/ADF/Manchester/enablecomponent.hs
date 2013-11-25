{-# LANGUAGE DoAndIfThenElse #-}
module DataflowBook.ADF.Manchester.EnableComponent

(enableMT, enableST)
where

import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.TokenOperations as Token
import qualified DataflowBook.ADF.Dataflow.Program as Program
import qualified DataflowBook.ADF.Dataflow.Node as Node
import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.ActivationOperations as Act
import qualified DataflowBook.ADF.Manchester.ExeToken as ExeToken

import Data.IORef
import Control.Concurrent

-----------
-- Enabler
-----------

enableMT :: (Eq t, Show t) => IORef [(Token.T t)] -> Chan (Token.T t) -> Chan (ExeToken.ExeToken t) -> (Program.T t) ->  IO()
enableMT ip_store input output env = do
    input_ip <- readChan input
    tokens <- enableST ip_store input_ip env
    mapM_ (writeChan output) tokens
    enableMT ip_store input output env


enableST :: (Eq t, Show t) => IORef [(Token.T t)] -> (Token.T t) -> (Program.T t) -> IO([ExeToken.ExeToken t])
enableST ip_store_ref input_ip env = do
    ip_store_old <- readIORef ip_store_ref
    writeIORef ip_store_ref (ip_store_old ++ [input_ip]) -- append new ip to ip_store

    maybe_token <- enableOne ip_store_ref (Token.getNode input_ip) env
    case maybe_token of
        Nothing -> return []
        Just t -> return [t]



enableOne :: (Show t, Eq t) => IORef [Token.T t] -> Id.NodeId -> Program.T t -> IO(Maybe (ExeToken.ExeToken t))
enableOne ip_store_ref nodeid env = do
    ip_store <- readIORef ip_store_ref
    let nodeips = Token.matchingNode nodeid ip_store

    let maybe_node = Node.findNode nodeid $ Program.getNodes env
    case maybe_node of
        Nothing -> do
            return (Nothing)

        Just node -> do
            let (activated,used_ips,unused_ips) = Act.isFireable node nodeips env

            if activated then do
                -- Build ExeToken and send
                let token = ExeToken.ExeToken node used_ips

                -- Remove all IPs belonging to the node
                let ip_store2 = filter (\ip -> Token.getNode ip /= nodeid) ip_store
                let ip_store3 = unused_ips ++ ip_store2 -- add back unused Token in same order

                writeIORef ip_store_ref ip_store3

                return (Just token)

            else
                return (Nothing)
