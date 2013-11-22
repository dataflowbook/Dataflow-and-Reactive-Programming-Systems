module DataflowBook.ADF.Dataflow.FirePattern
(T(Exists,Empty,DontCare,Always)
,getPatternPort
)where

import qualified DataflowBook.ADF.Dataflow.Port as Port

data T
    = Exists Port.T
    | Empty Port.T
    | DontCare Port.T
    | Always
    deriving Show

getPatternPort :: T -> Port.T
getPatternPort pat =
    case pat of
        Exists p -> p
        Empty p -> p
        DontCare p -> p
