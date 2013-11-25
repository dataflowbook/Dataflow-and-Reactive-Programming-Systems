module DataflowBook.ADF.Dataflow.Def

(T, mk
,getFn,getPorts,getFirePattern
)
where

import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.Port as Port
import qualified DataflowBook.ADF.Dataflow.FirePattern as FirePattern
import qualified DataflowBook.ADF.Dataflow.Local as Local


data T t = Def ([Local.T t] -> [Local.T t]) [Port.T] [[FirePattern.T]]

instance Show (T t) where
    show (Def _ ports pats) = "Def " ++ (show ports) ++ (show pats)

mk = Def

getFn (Def fn _ _) = fn

getPorts (Def _ ps _) = ps

getFirePattern (Def _ _ pat) = pat