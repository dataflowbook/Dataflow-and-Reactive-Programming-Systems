module DataflowBook.SDF.Schedule.ScheduleOperations where

import DataflowBook.SDF.Schedule.Index
import DataflowBook.SDF.Schedule.TokenCount
import DataflowBook.SDF.Schedule.ArcCapacity
import DataflowBook.SDF.Schedule.FireCount
import DataflowBook.SDF.Schedule.ActivationMatrix
import DataflowBook.SDF.Schedule.ActivationVector
import DataflowBook.SDF.Schedule.Topo
import DataflowBook.SDF.Schedule.InstantState
import DataflowBook.SDF.Schedule.AnalysisResult
import DataflowBook.SDF.Schedule.Graph
import qualified DataflowBook.SDF.SystemOfEquations.FireCountEq as Equation

import Data.Array.IArray
import Debug.Trace
import Control.Monad
import qualified Data.List as Lst
import qualified Data.List.Split as Lst


----------------------------------
-- Initial State Helper Functions
----------------------------------

initialsArcFull :: Topo -> TokenCount
initialsArcFull topo = array (firstIdx,arcCount)
    [ (a, amount)
    | a <- [firstIdx .. arcCount]
    , let amount = abs $ minimum [ topo!(n,a)  | n <- [firstIdx .. nodeCount]]
    ]
    where
        nodeCount = nodebounds topo
        arcCount = arcbounds topo

initialsDoubleFull :: Topo -> TokenCount
initialsDoubleFull topo = array (firstIdx,arcCount)
    [ (a, amount)
    | a <- [firstIdx .. arcCount]
    , let amount = sum [abs $ topo!(n,a)  | n <- [firstIdx .. nodeCount]]
    ]
    where
        nodeCount = nodebounds topo
        arcCount = arcbounds topo


----------------
-- Arc Capacity
----------------

-- | Determine the max token count per arc
-- | To maximize node activation we set the total to be
-- | the sum of the abs value of an arc's row in the Topo matrix
-- | The other option is; max abs value of an arc's row
arcCapacityVector :: (Topo->ArcIndex->Int) -> Topo -> ArcCapacity
arcCapacityVector fn topo = array (firstIdx,arcCount)
    [ (a, arcsize)
    | a <- [firstIdx .. arcCount]
    , let arcsize = fn topo a
    ]
    where
        arcCount = arcbounds topo

sizeSumOfAbs :: Topo -> ArcIndex -> Int
sizeSumOfAbs topo a =
    sum [ abs $ topo!(n,a) | n <- [firstIdx .. nodeCount] ]
    where
        nodeCount = nodebounds topo

sizeMaxOfAbs :: Topo -> ArcIndex -> Int
sizeMaxOfAbs topo a =
    maximum [ abs $ topo!(n,a) | n <- [firstIdx .. nodeCount] ]
    where
        nodeCount = nodebounds topo

givenArcCapacity :: Int -> Topo -> ArcIndex -> Int
givenArcCapacity capacity _ _ = capacity

--------------
-- Activation
--------------

-- | Create an Activation Matrix. 1 means activated, 0 otherwise
activationMatrix :: Topo -> TokenCount -> ArcCapacity -> ActivationMatrix
activationMatrix topo current arcmax = array ((firstIdx,firstIdx),(nodeCount,arcCount))
    [
        if is_input then
            if is_enough_tokens then
                ((n,a), 1)
            else
                ((n,a), 0)
        else if is_output then
            if is_not_full then
                ((n,a), 1)
            else
                ((n,a), 0)
        else ((n,a), 0)

    | n <- [firstIdx .. nodeCount]
    , a <- [firstIdx .. arcCount]
    , let is_input = topo!(n,a) < 0
    , let is_output = topo!(n,a) > 0
    , let not_connected = topo!(n,a) == 0
    , let is_enough_tokens = current!a >= abs (topo!(n,a))
    , let is_not_full = (current!a) + (topo!(n,a)) <= (arcmax!a)
    ]

    where
        nodeCount = nodebounds topo
        arcCount = arcbounds topo

-- | Build an Activation Vector from an Activation Matrix
activationVector :: Topo -> ActivationMatrix -> ActivationVector
activationVector topo actmatrix = array (firstIdx,nodeCount)
    [ if is_activated then (n,1) else (n,0)

    | n <- [firstIdx .. nodeCount]
    , let is_activated = and [ actmatrix!(n,a) == 1  | a<-[firstIdx..arcCount], topo!(n,a) /= 0]
    ]

    where
        nodeCount = nodebounds topo
        arcCount = arcbounds topo


----------------------
-- Simulation Updates
----------------------

-- | Create an updated arc token count matrix
updateTokenCount :: Topo -> ActivationVector -> TokenCount -> TokenCount
updateTokenCount topo actvector oldsize = array (firstIdx,arcCount)
    [ (a, oldsize!a + adjustment)
    | a <- [firstIdx .. arcCount]
    , let adjustment = sum [ topo!(n,a) * actvector!n | n<-[firstIdx..nodeCount]  ]
    ]
    where
        nodeCount = nodebounds topo
        arcCount = arcbounds topo


-- | Create Fire Count Vector (total # of times a node has been fired)
updateFireCount :: Topo -> ActivationVector -> FireCount -> FireCount
updateFireCount topo actvector oldfire = array (firstIdx,nodeCount)
    [ (n, oldfire!n + actvector!n) | n<-[firstIdx..nodeCount] ]
    where
        nodeCount = nodebounds topo


--------------------
-- Error Conditions
--------------------

-- | True if any arc is over full
isOverflowed :: TokenCount -> ArcCapacity -> Bool
isOverflowed current arcmax =
    or [ current!a > arcmax!a | a <- [firstIdx .. arcCount] ]
    where
        arcCount = ubounds current

-- | True if any arc has a negative token count
isUnderflowed :: TokenCount -> Bool
isUnderflowed current =
    or [ current!a < 0 | a <- [firstIdx .. arcCount] ]
    where
        arcCount = ubounds current

-- | True if no node was fired in the Activation Vector
isDeadlocked :: ActivationVector -> Bool
isDeadlocked actvector =
    and [ actvector!n == 0 | n<-[firstIdx..nodeCount] ]

    where
        nodeCount = ubounds actvector

isAllFired :: FireCount -> Bool
isAllFired firecount =
    and [ firecount!(n) > 0 | n <- [firstIdx .. (ubounds firecount)] ]


------------------------
-- Execution Simulation
------------------------

-- | Analyse the result of a simulation for errors
analyse :: Graph -> [InstantState] -> AnalysisResult
analyse graph history =
        analyse' graph (drop 1 history)
            -- drop first elem in history because it is our initial
            -- state and should not be tested
            -- Reverse it so we are testing in chronological order

analyse' :: Graph -> [InstantState] -> AnalysisResult
analyse' _ [] = undefined -- case handled by other case function
analyse' graph history =
    let
        state : rest_state = history
        topo = getTopo graph

        overflow_condition = isOverflowed (getTokenCount state) (getArcCapacity graph)
        underflow_condition = isUnderflowed (getTokenCount state)
        deadlock_condition = isDeadlocked (getActivationVector state)
        isAllFired_condition = isAllFired (getFireCount state)
    in
        if overflow_condition then (Overflow history)

        else if underflow_condition then (Underflow history)

        else if deadlock_condition then (Deadlock history)

        else if (null rest_state) then
            if isAllFired_condition then
                Success
            else
                NoFire

        else analyse' graph rest_state

-- | Returns a list of states (last element is most recent state, head
-- | is the first) that are snap-shots of the state of the dataflow program
-- | as it is being simulated
simulate :: (Graph->InstantState->ActivationVector) -> Graph -> Int -> [InstantState]
simulate fn graph max_iterations =
    let
        topo = getTopo graph
        nodeCount = nodebounds topo
        arcCount = arcbounds topo
        instant_count = 0 -- Initial instance state number

        state = InstantState (getInitialState graph) (emptyFireCount nodeCount) (emptyActivationVector nodeCount) instant_count
    in
        reverse $ simulate' fn graph [state] max_iterations
            -- must reverse list so head is the earliest state

simulate' :: (Graph->InstantState->ActivationVector) -> Graph -> [InstantState] -> Int -> [InstantState]
simulate' fn graph states max_iterations =
    let
        state = head states
            -- WARNING: head, precondition that 'states' may never be empty
            -- and head is always the most current state

        new_state = oneInstant fn graph state

        new_history = new_state : states

        instant_number = getInstantNumber state

    in
        if (instant_number >= max_iterations) then
            new_history
        else
            simulate' fn graph new_history max_iterations

-- | Execute one "instant" and return new state
oneInstant :: (Graph->InstantState->ActivationVector) -> Graph -> InstantState -> InstantState
oneInstant fn graph state =
    let
        topo = getTopo graph

        activation_vector = fn graph state
        new_current = updateTokenCount topo activation_vector (getTokenCount state)
        new_firecount = updateFireCount topo activation_vector (getFireCount state)
        new_instance_number = (getInstantNumber state) + 1
    in
        InstantState new_current new_firecount activation_vector new_instance_number

-- | Search the results of a simulation for patterns of states
-- | to determine a schedule. One "period" is all the activations
-- | that the scheduler has to step through repeatedly.
findPeriod :: [InstantState] -> ([TokenCount],[ActivationVector])
findPeriod history  =
    let
        max_pattern_size  =  (length history) `div` (2::Int)
        found_patterns = map (findPeriod' history) [1 .. max_pattern_size]
        smallest_pattern = Lst.find (\(a,b) -> (not $ null a) && (not $ null b)) found_patterns
    in
        case smallest_pattern of
            Nothing -> ([],[])
            Just lst -> lst


findPeriod' :: [InstantState] -> Int -> ([TokenCount],[ActivationVector])
findPeriod' [] _ = ([],[])
findPeriod' history pattern_size =
    let
        is_history_large_enough = pattern_size <= 2 * (length history)

        set1 = take pattern_size history
        set2 = take pattern_size $ drop pattern_size history
        set1_tokens = map getTokenCount set1
        set2_tokens = map getTokenCount set2


        matched = (set1_tokens == set2_tokens)

        matched_token_pattern = set1_tokens
        matched_activation_pattern = map getActivationVector $ drop 1 $ take (pattern_size+1) history
            -- activation pattern is the same as set1 but
            -- it is shifted down one index
    in
        if is_history_large_enough then
            if matched then
                (matched_token_pattern,matched_activation_pattern)
            else
                findPeriod' (drop 1 history) pattern_size
        else
            ([],[])

-----------------------
-- System of Equations
-----------------------

buildEquations :: Topo -> [Equation.T]
buildEquations topo = 
    let
        firstIdx = firstIdx
        nodeCount = nodebounds topo
        arcCount = arcbounds topo
        
        mk_equations n1 n2 a = 
            [ Equation.unsolved n1 (n2 , abs ( fromIntegral(topo!(n2,a)) / fromIntegral(topo!(n1,a))) )
            , Equation.unsolved n2 (n1 , abs ( fromIntegral(topo!(n1,a)) / fromIntegral(topo!(n2,a))) )
            ]
    in
        concat
        [ eqs
        | n1 <- [firstIdx .. nodeCount]
        , a <- [firstIdx .. arcCount]
        , topo!(n1,a) /= 0
        , n2 <- [n1+1 .. nodeCount] 
        , topo!(n2,a) /= 0 
        , let eqs = mk_equations n1 n2 a
        ]


-------------------------------------------
-- Simulation Activation Vector Strategies
-------------------------------------------

-- | Use this when you are searching for a schedule (or period)
calculatedActivationVector :: Graph -> InstantState -> ActivationVector
calculatedActivationVector graph last_state =
    let
        topo = getTopo graph
        activation_matrix = activationMatrix topo (getTokenCount last_state) (getArcCapacity graph)
        activation_vector = activationVector topo activation_matrix
    in
        activation_vector

-- | Use this when you are simulating a set schedule
scheduleActivationVector :: [ActivationVector] -> Graph -> InstantState -> ActivationVector
scheduleActivationVector activation_pattern  graph last_state =
    let
        period_length = length activation_pattern

        last_instant_number = (getInstantNumber last_state)

        schedule_index = (last_instant_number `mod` period_length)
            -- we use the last instant number because the activation vector
            -- is always built from the prev instant for this instant

        vector = activation_pattern !! schedule_index
    in
        vector

------------------------
-- High-Level Interface
------------------------


-- | This is the high-level interface to the scheduling operations.
-- | It will simulate a graph and determine if it is error free.
-- | Then, it will attempt to find a repeating pattern in the states
-- | and take the smallest pattern found as the activation schedule.
-- | The found schedule is then tested to make sure there are no errors.
-- | The result is the TokenCount initial graph state and the
-- | list of activations to be used as the schedule.

findSchedule :: Graph -> Int -> Maybe (TokenCount,[ActivationVector])
findSchedule graph instant_count =
    let
        history = (simulate calculatedActivationVector graph instant_count)
        analysis = analyse graph history
        (pattern_tokens,pattern_activation) = findPeriod history

        schedule_test_initial_tokens = head pattern_tokens
        schedule_test_graph = Graph (getTopo graph) schedule_test_initial_tokens (getArcCapacity graph)
        schedule_test_history = simulate (scheduleActivationVector pattern_activation) schedule_test_graph instant_count
        schedule_test_analysis = analyse graph schedule_test_history
    in
        if ((isSuccess analysis) && (isSuccess schedule_test_analysis)) then
            Just (schedule_test_initial_tokens, pattern_activation)
        else
            Nothing


-- Private
-- Helper functions

arcbounds topo = snd $ snd $ bounds topo
nodebounds topo = fst $ snd $ bounds topo
ubounds vector = snd $ bounds vector
lbounds vector = fst $ bounds vector
