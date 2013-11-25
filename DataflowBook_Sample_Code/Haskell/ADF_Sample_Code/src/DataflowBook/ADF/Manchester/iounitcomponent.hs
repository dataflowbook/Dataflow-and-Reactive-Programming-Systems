module DataflowBook.ADF.Manchester.IOUnitComponent
(ioUnitMT,ioUnitST) where

import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.TokenOperations as TokenOp

import Control.Concurrent

------------
-- I/O Unit
------------

ioUnitMT :: (Token.T t -> Bool) -> Chan (Token.T t) -> Chan (Token.T t) -> Chan (Token.T t) -> IO()
ioUnitMT isInternal input output external_output = do
    input_ip <- readChan input

    let (ins,outs) = ioUnitST isInternal [input_ip]

    mapM_ (writeChan output) ins
    mapM_ (writeChan external_output) outs

    ioUnitMT isInternal input output external_output


ioUnitST :: (Token.T t -> Bool) -> [Token.T t] -> ([Token.T t], [Token.T t]) -- (internal ips, external ips)
ioUnitST isInternal input =
    foldl directInput ([],[]) input

    where
        directInput (ins,outs) ip =
            if (isInternal ip) then
                (TokenOp.pushBottom ip ins, outs)
            else
                (ins, TokenOp.pushBottom ip outs)

