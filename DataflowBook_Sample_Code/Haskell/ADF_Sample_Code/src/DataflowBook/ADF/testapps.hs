module DataflowBook.ADF.TestApps where

import qualified DataflowBook.ADF.Dataflow.Node as Node
import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.Edge as Edge
import qualified DataflowBook.ADF.Dataflow.Program as Program
import qualified DataflowBook.ADF.Dataflow.FirePattern as FirePattern
import qualified DataflowBook.ADF.Dataflow.Port as Port
import qualified DataflowBook.ADF.Dataflow.Val as Val
import qualified DataflowBook.ADF.Dataflow.Def as Def
import qualified DataflowBook.ADF.Dataflow.Local as Local

import Control.Monad.State
import Data.Maybe

--------------------
-- Node Definitions
--------------------

nodeAdd :: Def.T Int
nodeAdd = node2in1out (+)

nodeSubtract :: Def.T Int
nodeSubtract = node2in1out (-)

nodeMultiply :: Def.T Int
nodeMultiply = node2in1out (*)

nodeLogicalAnd :: Def.T Bool
nodeLogicalAnd = node2in1out (&&)

nodeLogicalOr :: Def.T Bool
nodeLogicalOr = node2in1out (||)

nodeLogicalNand :: Def.T Bool
nodeLogicalNand = node2in1out (\x y -> not (x && y) )

nodeLogicalNor :: Def.T Bool
nodeLogicalNor = node2in1out (\x y -> not (x || y) )

nodeLogicalXor :: Def.T Bool
nodeLogicalXor = node2in1out (xor)
    where
        xor = (/=)

nodeLogicalNot :: Def.T Bool
nodeLogicalNot = node1in1out not

nodeIncrementer :: Int -> Def.T Int
nodeIncrementer n = node1in1out (+n)

nodeConstant :: a -> Def.T a
nodeConstant constant = node0in1out constant

nodeId :: Def.T a
nodeId = node1in1out id

nodeMerge :: Def.T a
nodeMerge = nodeMaybe2in1out fn
    where
        fn maybe_a maybe_b =
            case (maybe_a,maybe_b) of
                (Nothing, Just _) -> fromJust maybe_b
                (Just _, Nothing) -> fromJust maybe_a
                (Just _,Just _) -> undefined

nodeMaybe2in1out :: (Maybe a -> Maybe a -> a) -> Def.T a
nodeMaybe2in1out fn = Def.mk
    (fn_maybe_2_in_1_out fn)
    [Port.newInput "A", Port.newInput "B", Port.newOutput "OUT"]
    [[FirePattern.Exists (Port.newInput "A")],[FirePattern.Exists (Port.newInput "B")]]

node2in1out :: (a->a->a) -> Def.T a
node2in1out fn = Def.mk
    (fn_2_in_1_out fn)
    [Port.newInput "A", Port.newInput "B", Port.newOutput "OUT"]
    [[FirePattern.Exists (Port.newInput "A"), FirePattern.Exists (Port.newInput "B")]]

node1in1out :: (a->a) -> Def.T a
node1in1out fn = Def.mk
    (fn_1_in_1_out fn)
    [Port.newInput "A", Port.newOutput "OUT"]
    [[FirePattern.Exists (Port.newInput "A")]]

node0in1out :: a -> Def.T a
node0in1out const = Def.mk
    (fn_0_in_1_out const)
    [Port.newOutput "OUT"]
    [[FirePattern.Always]]

-----------------
-- Test Programs
-----------------


app_dFlipFlop :: Program.T Bool
app_dFlipFlop = Program.new nodes edges initials 1
    where
        nodes =
            [Node.new "1" nodeLogicalNand
            ,Node.new "2" nodeLogicalNand
            ,Node.new "3" nodeLogicalNand
            ,Node.new "4" nodeLogicalNand
            ,Node.new "ConstantTrue" (nodeConstant True)
            ,Node.new "ConstantFalse" (nodeConstant False)
            ]
        edges =
            [Edge.new ("1","OUT") ("2","A")
            ,Edge.new ("1","OUT") ("3","A")
            ,Edge.new ("2","OUT") ("4","B")
            ,Edge.new ("3","OUT") ("4","A")
            ,Edge.new ("4","OUT") ("3","B")
            ,Edge.new ("3","OUT") ("EXT","Q")
            ,Edge.new ("4","OUT") ("EXT","Qnot")
            ,Edge.new ("ConstantTrue","OUT") ("1","A")
            ,Edge.new ("ConstantTrue","OUT") ("2","B")
            ,Edge.new ("ConstantTrue","OUT") ("1","B")
            ]
        initials =
            [Token.new ("3", "OUT") False
            ,Token.new ("4", "OUT") False
            ,Token.new ("1", "OUT") False
            ]

app_tree_test :: Program.T Int
app_tree_test = Program.new nodes edges initials 1
    where
        nodes =
            [Node.new "1" $ nodeIncrementer 1
            ,Node.new "2" $ nodeIncrementer 1
            ,Node.new "3" $ nodeIncrementer 2
            ,Node.new "4" $ nodeIncrementer 2
            ,Node.new "5" $ nodeIncrementer 3
            ,Node.new "6" $ nodeIncrementer 3
            ,Node.new "7" $ nodeIncrementer 4
            ]
        edges =
            [Edge.new ("1","OUT") ("2","A")
            ,Edge.new ("1","OUT") ("3","A")
            ,Edge.new ("2","OUT") ("4","A")
            ,Edge.new ("2","OUT") ("5","A")
            ,Edge.new ("3","OUT") ("6","A")
            ,Edge.new ("3","OUT") ("7","A")
            ,Edge.new ("4","OUT") ("EXT","4")
            ,Edge.new ("5","OUT") ("EXT","5")
            ,Edge.new ("6","OUT") ("EXT","6")
            ,Edge.new ("7","OUT") ("EXT","7")
            ]
        initials =
            [Token.new ("1", "A") 0]


-- Powers of "n" Program
app_powers_of_n :: Program.T Int
app_powers_of_n = Program.new nodes edges initials 1
    where
        nodes =
            [Node.new "1" nodeMultiply
            ,Node.new "n" nodeId
            ,Node.new "feedback" nodeId
            ,Node.new "feedback2" nodeId
            ]
        edges =
            [Edge.new ("n","OUT") ("1","B")
            ,Edge.new ("1","OUT") ("feedback","A")
            ,Edge.new ("1","OUT") ("EXT","OUT")
            ,Edge.new ("n","OUT") ("feedback2","A")
            ,Edge.new ("feedback","OUT") ("1","A")
            ,Edge.new ("feedback2","OUT") ("n","A")
            ]
        initials =
            [Token.new ("1","A") 1
            ,Token.new ("n","A") 2
            ]

app_merge_test :: Program.T String
app_merge_test = Program.new nodes edges initials 1
    where
        nodes =
            [Node.new "1a" nodeId
            ,Node.new "1b" nodeId
            ,Node.new "2a" nodeId
            ,Node.new "2b" nodeId
            ,Node.new "Merge" nodeMerge
            ]
        edges =
            [Edge.new ("1a","OUT") ("1b","A")
            ,Edge.new ("1b","OUT") ("1a","A")
            ,Edge.new ("2a","OUT") ("2b","A")
            ,Edge.new ("2b","OUT") ("2a","A")
            ,Edge.new ("1a","OUT") ("Merge","A")
            ,Edge.new ("2a","OUT") ("Merge","B")
            ,Edge.new ("Merge","OUT") ("EXT","OUT")
            ]
        initials =
            [Token.new ("1a","A") "Hello"
            ,Token.new ("2a","A") "World"
            ]

------------------
-- Node Fn Helpers
------------------

fn_2_in_1_out :: (t -> t -> t) -> [Local.T t] -> [Local.T t]
fn_2_in_1_out fn  inputs =
    case (maybe_a_input,maybe_b_input) of
        (Nothing, _) ->
            undefined
        (_, Nothing) ->
            undefined
        (Just tok_a, Just tok_b) ->
            [Local.new "OUT" output_val ]
            where
                output_val = fn a_input b_input
                a_input = Val.get $ Local.getVal tok_a
                b_input = Val.get $ Local.getVal tok_b

    where
        maybe_a_input = listToMaybe (getLocalsForPort (Port.newInput "A") inputs)
        maybe_b_input = listToMaybe (getLocalsForPort (Port.newInput "B") inputs)

fn_maybe_2_in_1_out :: (Maybe t -> Maybe t -> t) -> [Local.T t] -> [Local.T t]
fn_maybe_2_in_1_out fn  inputs =
    case (maybe_a_input,maybe_b_input) of
        (Nothing,Nothing) ->
            [Local.new "OUT" (fn Nothing Nothing)]

        (Nothing, Just tok_b) ->
            let
                b = Val.get $ Local.getVal tok_b
            in
                [Local.new "OUT" (fn Nothing (Just b))]

        (Just tok_a, Nothing) ->
            let
                a = Val.get $ Local.getVal tok_a
            in
                [Local.new "OUT" (fn (Just a) Nothing)]

        (Just tok_a, Just tok_b) ->
            let
                a = Val.get $ Local.getVal tok_a
                b = Val.get $ Local.getVal tok_b
            in
                [Local.new "OUT" (fn (Just a) (Just b))]
    where
        maybe_a_input = listToMaybe (getLocalsForPort (Port.newInput "A") inputs)
        maybe_b_input = listToMaybe (getLocalsForPort (Port.newInput "B") inputs)

fn_1_in_1_out :: (t -> t) -> [Local.T t] -> [Local.T t]
fn_1_in_1_out fn  inputs =
    case (maybe_a_input) of
        (Nothing) -> undefined -- Never should happen!!!

        (Just tok) ->
            [Local.new "OUT" output_val]
            where
                output_val = fn (Val.get $ Local.getVal tok)
    where
        maybe_a_input = listToMaybe (getLocalsForPort (Port.newInput "A") inputs)

fn_0_in_1_out :: t -> [Local.T t] -> [Local.T t]
fn_0_in_1_out constant inputs = [Local.new "OUT" constant]

pattern_2_in_1_out =
    [FirePattern.Exists (Port.newInput "A")
    ,FirePattern.Exists (Port.newInput "B")
    ]
pattern_1_in_1_out =
    [FirePattern.Exists (Port.newInput "A")
    ]
pattern_0_in_1_out =
    [FirePattern.Always]

getLocalsForPort :: Port.T -> [Local.T t] -> [Local.T t]
getLocalsForPort port locals =
    filter (\tok -> Local.getPort tok == port_id) locals
    where
        port_id = Node.getPortId port

