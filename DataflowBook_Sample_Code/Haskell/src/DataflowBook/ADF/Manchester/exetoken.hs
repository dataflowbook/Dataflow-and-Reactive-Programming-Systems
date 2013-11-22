module DataflowBook.ADF.Manchester.ExeToken where

import qualified DataflowBook.ADF.Dataflow.Node as Node
import qualified DataflowBook.ADF.Dataflow.Token as Token

data ExeToken t = ExeToken (Node.T t) [Token.T t]
    deriving Show
