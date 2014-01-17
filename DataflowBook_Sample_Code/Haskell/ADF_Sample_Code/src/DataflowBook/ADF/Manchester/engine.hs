module DataflowBook.ADF.Manchester.Engine

(runMT,runST)
where

import qualified DataflowBook.ADF.Dataflow.Token as Token
import qualified DataflowBook.ADF.Dataflow.Program as Program
import qualified DataflowBook.ADF.Manchester.ExeToken as ExeToken
import qualified DataflowBook.ADF.Dataflow.Id as Id
import qualified DataflowBook.ADF.Manchester.TransmitComponent as Tx
import qualified DataflowBook.ADF.Manchester.EnableComponent as Enable
import qualified DataflowBook.ADF.Manchester.ExecuteComponent as Execute
import qualified DataflowBook.ADF.Manchester.IOUnitComponent as IOUnit

import Control.Exception.Base
import Control.Monad
import Data.IORef
import Control.Concurrent

{-
    This based on the Manchester Dataflow Architecture with tweeks
    that make it more useful as a software dataflow engine.

    The engine itself is a very simple, dataflow pipeline architecture.
    To differentiate between code to implement this engine and code
    to model a dataflow program, we will call the nodes used by the engine
    "components" and reserve the term, "nodes" for the dataflow model types.
    "Links" are used in the engine while "edges" are used in the dataflow
    model.


    The components of this engine:

    Transmit Node: Moves the token from the output of one node to the input
            of another node. If there are multiple edges it can travel along,
            then the token value is duplicated and sent down each edge.

    Enable Node: Determines which nodes can be activated. Creates an
            Executable token that defines how to "run" the node and sends
            it to the the Execute Node

    Execute Node: Takes the ExeToken and then "runs" it. If the node
            generates output tokens, it sends them to the I/O Unit.

    I/O Unit: Accepts inputs from external sources and sends tokens to
            external processors.

    There are both multi-threaded (MT) and single-threaded (ST) versions
    of the components.

-}


----------------------
-- Internal Node Test
----------------------

isInternal :: Token.T t -> Bool
isInternal ip = (Id.mkNodeId "EXT") /= (Token.getNode ip)


------------------------
-- Multithreaded System
------------------------

{-
    In the multi-threaded version, channels are used between nodes
    to pass the data. They are may not exist in other languages but are
    easy enough to create you own. The single threaded version doesn't
    use channels and should be easy to port to other languages.
-}

runMT :: (Show t, Eq t) => Program.T t -> IO()
runMT prog =
    start prog (Program.getInitials prog)

start :: (Show t, Eq t) => (Program.T t) -> [(Token.T t)] -> IO()
start env initials  = do

    -- Create links between components
    linkto_tx <- newChan :: IO (Chan (Token.T t))
    linkto_outputIO <- newChan :: IO (Chan (Token.T t))
    linkto_exe <- newChan :: IO (Chan (ExeToken.ExeToken t))
    link_input_bypass <- newChan :: IO (Chan (Token.T t))
    linkto_inputIO <- newChan
    linkto_enable <- newChan
    linkto_externalOut <- newChan

    -- flag is set when a component process ends
    endflag <- newEmptyMVar :: IO (MVar ())

    -- Token Storage
    ip_store <- newIORef [] :: IO(IORef [(Token.T t)])

    -- inject initial values into IOUnit input link
    mapM (writeChan linkto_inputIO) initials

    -- Input Component
    forkFinally
        (IOUnit.ioUnitMT isInternal linkto_inputIO linkto_tx link_input_bypass)
        (onThreadEnd "IO UNIT (INPUT) ENDED" endflag)

    -- Transmit Component
    forkFinally
        (Tx.transmitMT ip_store linkto_tx linkto_outputIO env)
        (onThreadEnd "TRANSMIT ENDED" endflag)

    -- Output Component
    forkFinally
        (IOUnit.ioUnitMT (isInternal) linkto_outputIO linkto_enable linkto_externalOut)
        (onThreadEnd "IO UNIT (OUTPUT) ENDED" endflag)

    -- Enable Component
    forkFinally
        (Enable.enableMT ip_store linkto_enable linkto_exe env)
        (onThreadEnd "ENABLE ENDED" endflag)

    -- Execute Component
    forkFinally
        (Execute.executeMT linkto_exe linkto_tx)
        (onThreadEnd "EXECUTE ENDED" endflag)

    -- External Output Printer Component
    forkFinally
        (printer "EXTERNAL OUTPUT: " linkto_externalOut)
        (onThreadEnd "PRINTER ENDED" endflag)

    -- Input Bypass Printer Component
    forkFinally
        (printer "BYPASS: " link_input_bypass)
        (onThreadEnd "BYPASS ENDED" endflag)

    -- Null Input Component
    -- Needed because if no component is on the input to the Haskell Chan,
    -- it will report an error, deadlock
    forkFinally
        (nullInput linkto_inputIO)
        (onThreadEnd "NULL INPUT ENDED" endflag)

    --takeMVar endflag -- wait until at least one node terminates, then end program
    waitForever

onThreadEnd :: (Show a) => String -> MVar () ->  Either SomeException a -> IO()
onThreadEnd msg flag except = do
        putStrLn $ msg ++ " " ++ show except
        putMVar flag ()

printer :: (Show t) => String -> Chan (Token.T t) -> IO()
printer msg input = do
    ip <- readChan input
    putStrLn $ msg ++ show ip
    printer msg input

nullInput :: Chan (Token.T t) -> IO()
nullInput output = do
    threadDelay 100000
    nullInput output

waitForever = do
    threadDelay 10000
    waitForever

------------------------
-- Single-Thread System
------------------------

-- | Single threaded single step flag
single_step = False

-- | Run the Dataflow program
runST :: (Show t, Eq t) => Program.T t -> IO()
runST prog =
    startLoop prog (Program.getInitials prog)

startLoop :: (Show t, Eq t)
    => (Program.T t) -- The dataflow program to run
    -> [(Token.T t)] -- Initial tokens
    -> IO()
startLoop env initials= do
    ip_store <- newIORef [] :: IO(IORef[(Token.T t)])
    startLoop' env initials ip_store

    where
    startLoop' :: (Show t, Eq t)
        => (Program.T t)         -- Dataflow program to run
        -> [(Token.T t)]         -- Initial tokens
        -> IORef [(Token.T t)]   -- Reference to token store
        -> IO()
    startLoop' env initials ip_store = do

        -- -------------------------------------------------------------------
        step "IO UNIT (INPUT):" initials
        let (link1, _) = IOUnit.ioUnitST (isInternal) initials

        -- -------------------------------------------------------------------
        step "TX:" link1
        link2a <- mapM (\ip -> Tx.transmitST ip_store ip env) link1
        let link2b = concat link2a

        -- -------------------------------------------------------------------
        step "IO UNIT(OUTPUT)" link2b
        let (link2_1,external_outs) = IOUnit.ioUnitST (isInternal) link2b

        putStrLn $ "EXTERNAL OUTPUT: " ++ show external_outs

        -- -------------------------------------------------------------------
        step "ENABLE" link2_1
        link3 <- mapM (\ip -> Enable.enableST ip_store ip env) link2_1
        let link3b = concat link3

        -- -------------------------------------------------------------------
        step "EXECUTE" link3b
        link4 <- mapM Execute.executeST link3b
        let link4b = concat link4

        startLoop' env link4b ip_store

        where
            step name a = do
                when single_step $ do
                    ips <- readIORef ip_store
                    stepPause (name ++ ": " ++ show a ++ "\n" ++ "Token-STORE: " ++ show ips)


stepPause :: String -> IO ()
stepPause msg = do
    putStrLn "=========================================="
    putStrLn msg
    putStrLn "=========================================="
    getChar
    return ()
