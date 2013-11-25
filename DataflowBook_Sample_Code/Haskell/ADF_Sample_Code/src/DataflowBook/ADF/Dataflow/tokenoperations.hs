module DataflowBook.ADF.Dataflow.TokenOperations


where

import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.Val as Val
import qualified DataflowBook.ADF.Dataflow.Local as Local
import qualified DataflowBook.ADF.Dataflow.Token as Token

import qualified Data.List as Lst

----------
-- Locals
----------

localToGlobal :: Id.NodeId -> [Local.T t] -> [Token.T t]
localToGlobal id locals =
    case locals of
        [] -> []
        (l:ls) ->
            let
                portid = Local.getPort l
                val = Local.getVal l
            in
                (Token.mk id portid val) : (localToGlobal id ls)


globalToLocal :: [Token.T t] -> [Local.T t]
globalToLocal globals =
    case globals of
        [] -> []
        (g:gs) ->
            let
                portid = Token.getPort g
                val = Token.getVal g
            in
                (Local.mk portid val) : (globalToLocal gs)

--------------------
-- Stack Operations
--------------------

removeTopMost :: (Eq t) => Token.T t -> [Token.T t] -> [Token.T t]
removeTopMost ip ips = Lst.delete ip ips

removeAllTopMost :: (Eq t) => [Token.T t] -> [Token.T t] -> [Token.T t]
removeAllTopMost removes lst =
    case removes of
        [] -> lst
        (this:rest) -> removeAllTopMost rest (removeTopMost this lst)

pushBottom :: Token.T t -> [Token.T t] -> [Token.T t]
pushBottom ip ips =
    _pushBottom ip ips

pushAllBottom :: [Token.T t] -> [Token.T t] -> [Token.T t]
pushAllBottom input_ips ips =
    ips ++ input_ips

_pushBottom :: a -> [a] -> [a]
_pushBottom x xs =
    case xs of
        [] -> [x]
        (this:rest) -> this : (_pushBottom x rest)

removeFirst :: (Eq t) => Token.T t -> [Token.T t] -> [Token.T t]
removeFirst ip ips = Lst.delete ip ips

removeAll :: (Eq t) => Token.T t -> [Token.T t] -> [Token.T t]
removeAll ip ips = filter (== ip) ips


-------------
-- Searching
-------------

matchingNode :: Id.NodeId -> [Token.T t] -> [Token.T t]
matchingNode nodeid lst = filter (\ip -> nodeid == (Token.getNode ip)) lst

matchingNodePort :: Id.NodeId -> Id.PortId -> [Token.T t] -> [Token.T t]
matchingNodePort nodeid portid lst = filter (\ip -> nodeid == (Token.getNode ip) && portid == (Token.getPort ip)) lst

-- Returns the first IP for each port matching node and port
firstMatchingNodePorts :: Id.NodeId -> [Id.PortId] -> [Token.T t] -> [Token.T t]
firstMatchingNodePorts nodeid ports lst =
    (concat . map (take 1) . map (\p -> matchingNodePort nodeid p lst)) ports
