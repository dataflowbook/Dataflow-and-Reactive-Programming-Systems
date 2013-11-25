{-# LANGUAGE FlexibleInstances #-}

module DataflowBook.SDF.Schedule.InstantState where

import DataflowBook.SDF.Schedule.TokenCount
import DataflowBook.SDF.Schedule.FireCount
import DataflowBook.SDF.Schedule.ActivationVector
import DataflowBook.SDF.Schedule.Pretty

data InstantState =
    InstantState
        TokenCount       -- Current Token Counts
        FireCount        -- Fire Count
        ActivationVector -- Activation that produced this token and fire count
        Int              -- The current instant number (incremented for each 
                         --     new state). Instant 0 is always the initial 
                         --     state.
    deriving (Show, Eq)

getTokenCount (InstantState a _ _ _) = a
getFireCount (InstantState _ a _ _) = a
getActivationVector (InstantState _ _ a _) = a
getInstantNumber (InstantState _ _ _ a) = a

instance PrettyPrint InstantState where

    prettyPrint state =
        "Instance: " ++ (show $ getInstantNumber state) ++ "\n"
        ++
        "Activation: \t\t" ++ (show $ getActivationVector state) ++ "\n"
        ++
        "Token Count: \t\t" ++ (show $ getTokenCount state) ++ "\n"
        ++
        "Fire Count: \t\t" ++ (show $ getFireCount state) ++ "\n"

instance PrettyPrint ([InstantState]) where

    prettyPrint [] = "End\n"
    prettyPrint (s:ss) =
        "----------------------------------------------------------------------\n"
        ++
        (prettyPrint s)
        ++
        "----------------------------------------------------------------------\n"
        ++
        (prettyPrint ss)
