module DataflowBook.ADF.Dataflow.Local

(T
,mk,new
,getPort,getVal
)
where

import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Dataflow.Val as Val

data T t = Local Id.PortId (Val.T t)
    deriving Show

mk = Local

new :: String -> t -> T t
new portid val = Local (Id.mkPortId portid) (Val.mk val)

getPort (Local id _) = id

getVal (Local _ val) = val