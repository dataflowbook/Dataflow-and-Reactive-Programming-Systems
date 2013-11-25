module DataflowBook.SDF.Schedule.FireCount where

import DataflowBook.SDF.Schedule.Index

import Data.Array.IArray

type FireCount = Array NodeIndex Int
    -- 0 -> +inf

emptyFireCount :: NodeIndex -> FireCount
emptyFireCount nodeCount = array (firstIdx,nodeCount)
    [ (n, 0)
    | n <- [firstIdx .. nodeCount]
    ]
