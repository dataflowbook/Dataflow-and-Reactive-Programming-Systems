module DataflowBook.SDF.SystemOfEquations.FireCountEq 

(T,NodeId,FireCount,ArcRate
,unsolved,solved
,isSolved
,getSolution
,getAnswerNodeId
,getFoumula
)
where

type ArcRate = Double
type FireCount = Double
type NodeId = Int
type ThisNodeId = NodeId
type OtherNodeId = NodeId

type FireCountEq = T

data T
    = Unsolved ThisNodeId (OtherNodeId, ArcRate)
    | Solved ThisNodeId FireCount
    deriving (Show,Eq)


unsolved = Unsolved
solved = Solved

isSolved eq = 
    case eq of
        Solved _ _ -> True
        _ -> False

getSolution eq = 
    case eq of
        Unsolved _ _ -> Nothing
        Solved _ sol -> Just sol

getAnswerNodeId eq = 
    case eq of
        Solved nodeid _ -> nodeid
        Unsolved nodeid _ -> nodeid

getFoumula eq = 
    case eq of
        Unsolved _ form -> Just form
        _ -> Nothing