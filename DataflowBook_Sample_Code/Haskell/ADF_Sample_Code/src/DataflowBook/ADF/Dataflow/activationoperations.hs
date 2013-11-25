module DataflowBook.ADF.Dataflow.ActivationOperations
(
isFireable
) where

import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.Node as Node
import qualified DataflowBook.ADF.Dataflow.Program as Program
import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.Edge as Edge
import qualified DataflowBook.ADF.Dataflow.FirePattern as Pat
import qualified DataflowBook.ADF.Dataflow.TokenOperations as Token

import Data.List as Lst
import Data.Maybe


{-
    The function takes the node in question and all the available tokens
    that belong to that node, either at its input or output. It must return
    three things:
        - Bool: Is this node activated? Can it run?
        - A list of tokens that will used by the node
            taken from the input tokens
        - The list of unused tokens from the input
            *** MUST MAINTAIN TOKEN ORERING ***
    Play it safe, if a node is not activated then just return an empty list
    for the used tokens and pass the input tokens through unchanged.
-}

isFireable :: (Eq t) => (Node.T t) -> [(Token.T t)] -> Program.T t -> (Bool, [(Token.T t)], [(Token.T t)])
isFireable node ips prog =
    if (has_pattern_match && output_space_avail) then
        (True, just_matches, left_over_ips)
    else
        (False, [], [])

    where
        pats = Node.getPatterns node
        nodeid = Node.getId node
        (has_pattern_match, maybe_matches, left_over_ips) = head $ (filter (\(matched,_,_) -> matched) $ map (\pat -> allPatternMatch pat nodeid ips) pats) ++ [(False,[],[])]
        outport_ids = Node.getOutputPortIds node
        output_space_avail = all (<Program.getEdgeCapacity prog)  ( map (\p -> outputBacklog nodeid p ips prog) outport_ids)
        just_matches = catMaybes maybe_matches

outputBacklog :: Id.NodeId -> Id.PortId -> [Token.T t] -> Program.T t -> Int
outputBacklog nodeid portid ips prog =
{-
    count = 0
    get all edges that connect to the output port
    for each edge
        get all ips that are on the input end of edge
        count = count + ip-count
-}
    Lst.length input_ips
    where
        all_connected_edges = filter (\e -> (Edge.getSourceNode e) == nodeid && (Edge.getSourcePort e) == portid) (Program.getEdges prog)
        input_ips = concat $ map (\e -> Token.matchingNodePort (Edge.getSinkNode e) (Edge.getSinkPort e) ips) all_connected_edges

allPatternMatch :: (Eq t) => [Pat.T] -> Id.NodeId -> [Token.T t] -> (Bool,[Maybe (Token.T t)], [Token.T t])
allPatternMatch pats nodeid ips =
        foldl foldfn (True,[],ips) pats
    where
        foldfn :: (Eq t) => (Bool,[Maybe (Token.T t)], [Token.T t]) -> Pat.T -> (Bool,[Maybe (Token.T t)], [Token.T t])
        foldfn (last_success,last_matchs,last_ips) pat =
            if (last_success && this_success) then
                (True, this_match:last_matchs, this_ips)
            else
                (False,[],[])

            where
                (this_success,this_match,this_ips) = patternMatch pat nodeid last_ips

patternMatch :: (Eq t) => Pat.T -> Id.NodeId -> [Token.T t] -> (Bool, Maybe (Token.T t), [Token.T t])
patternMatch pat nodeid ips =
    case pat of
        Pat.Exists _ ->
            if (ip_count > 0) then
                (True, Just (head matching_ips), non_matching_ips ++ (drop 1 matching_ips))
            else
                (False, Nothing, ips)

        Pat.Empty _ ->
            if (ip_count == 0) then
                (True, Nothing, ips)
            else
                (False, Nothing, ips)

        Pat.DontCare _ ->
            if (0 < Lst.length matching_ips) then
                (True, Nothing, non_matching_ips)
            else
                (True, Just (head matching_ips), non_matching_ips ++ (drop 1 matching_ips))
        Pat.Always ->
            (True, Nothing, ips)

    where
        portid = Node.getPortId $ Pat.getPatternPort pat
        ip_count = Lst.length matching_ips
        matching_ips = Token.matchingNodePort nodeid portid ips
        non_matching_ips = ips \\ matching_ips
