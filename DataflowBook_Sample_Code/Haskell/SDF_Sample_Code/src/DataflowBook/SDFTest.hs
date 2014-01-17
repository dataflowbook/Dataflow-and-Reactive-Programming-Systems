module Main where
--module DataflowBook.SDFTest where


import DataflowBook.SDF.Schedule.ScheduleOperations
import DataflowBook.SDF.Schedule.ActivationVector
import DataflowBook.SDF.Schedule.Pretty
import DataflowBook.SDF.SampleGraphs



main :: IO()
main = do
    let success = findSchedule example 50
    case success of
        Just (initials, activations) -> do
            putStr "Success\n\n"
            putStr $ "Initials: \n" ++ (show initials) ++ "\n\n"
            putStr $ "Activation Vectors (In order from top to bottom)\n" ++ (prettyPrint activations)
        Nothing ->
            putStrLn "Unable to find a schedule\n"
