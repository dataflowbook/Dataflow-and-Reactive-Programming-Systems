module DataflowBook.ADF.Manchester.TransmitComponent
(transmitMT,transmitST

) where

import qualified DataflowBook.ADF.Dataflow.Edge as Edge
import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.Program as Program
import qualified DataflowBook.ADF.Dataflow.TokenOperations as TokenOp

import Data.IORef
import Control.Concurrent


------------
-- Transmit
------------

transmitMT :: (Show t) => IORef [(Token.T t)] -> Chan (Token.T t) -> Chan (Token.T t) -> (Program.T t) -> IO()
transmitMT ip_store input output env = do
    input_ip <- readChan input
    output_ips <- transmitST ip_store input_ip env
    mapM_ (writeChan output) output_ips

    transmitMT ip_store input output env

transmitST :: IORef [(Token.T t)] -> (Token.T t) -> (Program.T t) -> IO([(Token.T t)])
transmitST ip_store input_ip env = do
        ips <- readIORef ip_store
        return $ transmitOne input_ip env ips

isOnOutput :: (Token.T t) -> (Program.T t) -> Bool
isOnOutput tok env =
    let
        edges = Program.getEdges env
        nodeid = Token.getNode tok
        portid = Token.getPort tok
    in
        any (\e -> (Edge.getSourceNode e) == nodeid && (Edge.getSourcePort e) == portid) edges


transmitOne :: (Token.T t) -> (Program.T t) -> [(Token.T t)] -> [(Token.T t)]
transmitOne input_ip env ip_store =
    if all_space_available then
        if (is_on_output && all_space_available) then
            map (\edge -> changeOwner edge input_ip) edges
        else
            [input_ip]
    else
        [] -- NOTE: we are throwing away an Token!!!
    where
        is_on_output = isOnOutput input_ip env
        ip_count nodeid portid = length $ TokenOp.matchingNodePort nodeid portid ip_store
        edges = findEdgesForIP input_ip env
        all_space_available =
            if ((Program.getEdgeCapacity env) > 0) then
                all (\e -> (Program.getEdgeCapacity env) >= ip_count (Edge.getSinkNode e) (Edge.getSinkPort e)) edges
            else
                True

changeOwner :: Edge.T -> (Token.T t) -> (Token.T t)
changeOwner edge ip =
    Token.mk sink_node sink_port val

    where
        sink_node = Edge.getSinkNode edge
        sink_port = Edge.getSinkPort edge
        val = Token.getVal ip

findEdgesForIP :: (Token.T t) -> (Program.T t) -> [Edge.T]
findEdgesForIP ip env =
    let edges = Program.getEdges env in
    let ip_node = Token.getNode ip in
    let ip_port = Token.getPort ip in
    let is_match e = ip_node == (Edge.getSourceNode e) && ip_port == (Edge.getSourcePort e) in
    let matching_edges = filter is_match edges in
    matching_edges
