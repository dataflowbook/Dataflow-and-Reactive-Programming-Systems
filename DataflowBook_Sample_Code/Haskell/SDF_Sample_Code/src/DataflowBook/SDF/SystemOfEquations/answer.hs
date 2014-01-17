module DataflowBook.SDF.SystemOfEquations.Answer 

(T
,noAnswer,answer,working
,getSystemOfEquations
,isWorking
,isNoAnswer
,isAnswer
)
where

import qualified DataflowBook.SDF.SystemOfEquations.SysOfEq as SysOfEq 

type Answer = T

data T
    = NoAnswer SysOfEq.T
    | Answer SysOfEq.T
    | Working SysOfEq.T
    deriving (Show)

noAnswer = NoAnswer
answer = Answer
working = Working

getSystemOfEquations :: Answer -> SysOfEq.T
getSystemOfEquations ans = 
    case ans of
        NoAnswer sys -> sys
        Answer sys -> sys
        Working sys -> sys

isWorking :: Answer -> Bool
isWorking ans = 
    case ans of
        Working _ -> True
        _ -> False

isNoAnswer :: Answer -> Bool
isNoAnswer ans = 
    case ans of
        NoAnswer _ -> True
        _ -> False

isAnswer :: Answer -> Bool
isAnswer ans = 
    case ans of
        Answer _ -> True
        _ -> False