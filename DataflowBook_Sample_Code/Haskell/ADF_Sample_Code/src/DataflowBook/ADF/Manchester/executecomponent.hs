module DataflowBook.ADF.Manchester.ExecuteComponent

(executeMT, executeST)
where

import qualified DataflowBook.ADF.Manchester.ExeToken as ExeToken
import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.Node as Node
import qualified DataflowBook.ADF.Dataflow.TokenOperations as Token

import Control.Concurrent

-----------
-- Execute
-----------

executeMT :: (Show t) => Chan (ExeToken.ExeToken t) -> Chan (Token.T t) -> IO()
executeMT input output = do
    token <- readChan input
    output_globals <- executeST token
    mapM_ (writeChan output) output_globals
    executeMT input output


executeST :: (ExeToken.ExeToken t) -> IO [(Token.T t)]
executeST token = do
    let ExeToken.ExeToken node globals = token
    let activation_fn = Node.getFn node
    let locals = Token.globalToLocal globals
    let output_locals = activation_fn locals
    let nodeid = Node.getId node
    let output_globals = Token.localToGlobal nodeid output_locals
    return output_globals
