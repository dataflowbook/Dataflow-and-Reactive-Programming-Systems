module DataflowBook.SDF.Schedule.Pretty where

class PrettyPrint a where
    prettyPrint :: a -> String
