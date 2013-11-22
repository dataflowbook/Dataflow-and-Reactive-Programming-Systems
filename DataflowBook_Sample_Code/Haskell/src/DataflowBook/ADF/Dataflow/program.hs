module DataflowBook.ADF.Dataflow.Program

(T, new
,getNodes, getEdges, setNodes, setEdges
,getEdgeCapacity, setEdgeCapacity
,getInitials, setInitials
) where

import qualified DataflowBook.ADF.Dataflow.Node as Node
import qualified DataflowBook.ADF.Dataflow.Edge as Edge
import qualified DataflowBook.ADF.Dataflow.Token as Token


data T t = Program (Nodes t) Edges [Token.T t] EdgeCapacity
    deriving Show

newtype Nodes t = NS [Node.T t]
    deriving Show

newtype Edges = ES [Edge.T]
    deriving Show

newtype EdgeCapacity = Capacity Int
    deriving Show


mkNodes = NS
mkEdges = ES
mkCapacity = Capacity

-- | Constructor
new :: [Node.T t] -> [Edge.T] -> [Token.T t] -> Int -> T t
new ns es toks cap = Program (mkNodes ns) (mkEdges es) toks (mkCapacity cap)


-----------
-- Get/Set
-----------

getNodes :: T t -> [Node.T t]
getNodes (Program (NS ns) _ _ _) = ns

getEdges :: T t -> [Edge.T]
getEdges (Program _ (ES es) _ _) = es

setNodes :: [Node.T t] -> T t -> T t
setNodes ns (Program _ edges init cap) = Program (NS ns) edges init cap

setEdges :: [Edge.T] -> T t -> T t
setEdges edges (Program ns _ init cap) = Program ns (ES edges) init cap

getEdgeCapacity :: T t -> Int
getEdgeCapacity (Program _ _ _ (Capacity n)) = n

setEdgeCapacity :: Int -> T t -> T t
setEdgeCapacity n (Program ns es init _) = Program ns es init (Capacity n)

getInitials :: T t -> [Token.T t]
getInitials (Program _ _ initials _) = initials

setInitials :: [Token.T t] -> T t -> T t
setInitials initials (Program ns es _ cap) = Program ns es initials cap
