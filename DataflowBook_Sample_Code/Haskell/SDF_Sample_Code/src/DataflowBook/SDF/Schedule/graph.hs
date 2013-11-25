module DataflowBook.SDF.Schedule.Graph where

import DataflowBook.SDF.Schedule.Topo
import DataflowBook.SDF.Schedule.TokenCount
import DataflowBook.SDF.Schedule.ArcCapacity


data Graph = 
    Graph 
        Topo        -- Topo matrix of graph
        TokenCount  -- Initial Arc State
        ArcCapacity -- Arc Capacity vector


getTopo (Graph a _ _) = a
getInitialState (Graph _ a _) = a
getArcCapacity (Graph _ _ a) = a
