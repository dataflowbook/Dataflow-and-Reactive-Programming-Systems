module DataflowBook.EquationsTest where

import qualified DataflowBook.SDF.SystemOfEquations.FireCountEq as Equation
import qualified DataflowBook.SDF.SystemOfEquations.SysOfEq as SysOfEq
import qualified DataflowBook.SDF.SystemOfEquations.Answer as Answer
import DataflowBook.SDF.SampleEquations
import qualified DataflowBook.SDF.SystemOfEquations.Solve as Solve

test1 = Answer.isNoAnswer (Solve.solve badsys)
test2 = Answer.isAnswer (Solve.solve goodsys)

