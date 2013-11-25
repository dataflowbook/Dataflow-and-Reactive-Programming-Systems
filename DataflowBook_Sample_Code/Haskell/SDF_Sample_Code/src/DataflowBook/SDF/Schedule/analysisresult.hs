module DataflowBook.SDF.Schedule.AnalysisResult where

import DataflowBook.SDF.Schedule.InstantState
import DataflowBook.SDF.Schedule.Pretty

data AnalysisResult
    = Success
    | NoFire
    | Overflow [InstantState]
    | Underflow [InstantState]
    | Deadlock [InstantState]
    deriving Show

isSuccess res = 
    case res of
        Success -> True
        _ -> False


instance PrettyPrint AnalysisResult where

    prettyPrint result = 
        case result of
            Success -> "Success\n"
            NoFire -> "NoFire\n"
            Overflow h -> "Overflow\n" ++ (prettyPrint h)
            Underflow h -> "Overflow\n" ++ (prettyPrint h)
            Deadlock h -> "Deadlock\n" ++ (prettyPrint h)
