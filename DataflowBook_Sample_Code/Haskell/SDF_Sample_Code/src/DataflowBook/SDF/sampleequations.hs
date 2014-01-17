module DataflowBook.SDF.SampleEquations where

import qualified DataflowBook.SDF.SystemOfEquations.FireCountEq as Equation
import qualified DataflowBook.SDF.SystemOfEquations.SysOfEq as SysOfEq


_A = 1
_B = 2
_C = 3
_D = 4
_E = 5

goodsys :: SysOfEq.T
goodsys = [nodeD,nodeE,nodeC,nodeB1,nodeB2,nodeA]
    where
        nodeD = Equation.solved _D 1.0
        nodeE = Equation.unsolved _E (_D,1)
        nodeC = Equation.unsolved _C (_D,2)
        nodeB1 = Equation.unsolved _B (_D,1)
        nodeB2 = Equation.unsolved _B (_C, 0.5)
        nodeA = Equation.unsolved _A (_B, 2.0)

badsys :: SysOfEq.T
badsys = [nodeD,nodeE,nodeC,nodeB1,nodeB2,nodeA]
    where
        nodeD = Equation.solved _D 1.0
        nodeE = Equation.unsolved _E (_D,1)
        nodeC = Equation.unsolved _C (_D,1)
        nodeB1 = Equation.unsolved _B (_D,1)
        nodeB2 = Equation.unsolved _B (_C, 0.5)
        nodeA = Equation.unsolved _A (_B, 2.0)
