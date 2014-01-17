module DataflowBook.SDF.SystemOfEquations.SysOfEq 

(T
,soeGetSolutions
,soeHasSolutions
,assign
)
where

import qualified DataflowBook.SDF.SystemOfEquations.FireCountEq as Equation

type SysOfEq = T
type T = [Equation.T]

soeGetSolutions :: SysOfEq -> Equation.NodeId -> [Equation.T]
soeGetSolutions sys nodeid = filter Equation.isSolved $ filter (\a -> nodeid == (Equation.getAnswerNodeId a)) sys
    
soeHasSolutions :: SysOfEq -> Equation.NodeId -> Bool
soeHasSolutions sys id = 0 < (length $ soeGetSolutions sys id)

assign :: Equation.NodeId -> Equation.FireCount -> SysOfEq -> SysOfEq
assign nodeid val sys =
    map (\eq -> if (nodeid == Equation.getAnswerNodeId eq) then Equation.solved nodeid val else eq) sys
