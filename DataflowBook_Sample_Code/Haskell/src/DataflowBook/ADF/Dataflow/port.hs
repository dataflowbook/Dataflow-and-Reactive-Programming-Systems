module DataflowBook.ADF.Dataflow.Port
(T
,mkInput,mkOutput,newInput,newOutput
,getId
,isInput,isOutput
) where

import qualified DataflowBook.ADF.Dataflow.Id as Id

data T
    = Input Id.PortId
    | Output Id.PortId
    deriving Show

mkInput = Input
mkOutput = Output

newInput :: String -> T
newInput n = Input (Id.mkPortId n)

newOutput :: String -> T
newOutput n = Output (Id.mkPortId n)

getId (Input id) = id
getId (Output id) = id

isOutput (Input _) = False
isOutput (Output _) = True

isInput (Input _) = True
isInput (Output _) = False