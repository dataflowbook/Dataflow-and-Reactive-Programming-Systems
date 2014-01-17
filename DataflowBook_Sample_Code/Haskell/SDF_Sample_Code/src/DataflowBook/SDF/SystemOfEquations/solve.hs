module DataflowBook.SDF.SystemOfEquations.Solve

--(solve)
where

import qualified DataflowBook.SDF.SystemOfEquations.FireCountEq as Equation
import qualified DataflowBook.SDF.SystemOfEquations.SysOfEq as SysOfEq
import qualified DataflowBook.SDF.SystemOfEquations.Answer as Answer

import Data.Maybe
import qualified Data.List as Lst

solve :: SysOfEq.T -> Answer.T
solve sys =
    let
        (is_deadlock,newsys) = solveOnePass sys
        onepass = (validate  . simplify) newsys
    in
        if is_deadlock
            then (Answer.noAnswer $ Answer.getSystemOfEquations onepass)
            else if (Answer.isWorking onepass)
                then (solve $ Answer.getSystemOfEquations onepass)
                else onepass

solveOnePass :: SysOfEq.T -> (Bool,SysOfEq.T)
solveOnePass sys = solveOnePass' sys (sys,[])

solveOnePass' :: SysOfEq.T -> (SysOfEq.T,SysOfEq.T) -> (Bool,SysOfEq.T)
solveOnePass' original ([],viewed) =
    if (null $ original Lst.\\ viewed)
        then (True,viewed) -- deadlock
        else (False,viewed) -- ok

solveOnePass' original ((eq:unviewed),viewed) =
    if (Equation.isSolved eq)
        then solveOnePass' original (unviewed,(eq:viewed))
        else solveOnePass' original (unviewed,(attempts ++ viewed))
            where
                attempts = attemptToSolve original eq

attemptToSolve sys equation =
    if already_solved
        then [equation]
        else
            if (SysOfEq.soeHasSolutions sys otherid)
                then map (\sol ->Equation.solved thisid (arcrate * (fromJust $ Equation.getSolution sol)) )(SysOfEq.soeGetSolutions sys otherid)
                else [equation]
    where
        already_solved = Equation.isSolved equation
        (otherid,arcrate) = fromJust $ Equation.getFoumula equation
        thisid = Equation.getAnswerNodeId equation

simplify :: SysOfEq.T -> SysOfEq.T
simplify = Lst.nub
    -- remove duplicate and equal solutions for the same nodeid

validate :: SysOfEq.T -> Answer.T
validate sys =
    if unsolvable
        then (Answer.noAnswer sys)
        else if done
            then (Answer.answer sys)
            else (Answer.working sys)
    where
        -- true if multiple unequal solutions for one nodeid
        unsolvable =
            any (\a -> (length a) > 1)
            $ map Lst.nub
            $ map (filter Equation.isSolved)
            $ Lst.groupBy (\a b -> (Equation.getAnswerNodeId a) == (Equation.getAnswerNodeId b))
            $ Lst.sortBy (\a b -> compare (Equation.getAnswerNodeId a) (Equation.getAnswerNodeId b)) sys

        -- true if all eqs are solved
        done = all Equation.isSolved sys






