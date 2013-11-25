module DataflowBook.SDF.Schedule.Index where

type NodeIndex = Int
    -- 0 -> +inf

type ArcIndex = Int
    -- 0 -> +inf

-- | Determines the lower bounds for array indexing
firstIdx :: Int
firstIdx = 0
