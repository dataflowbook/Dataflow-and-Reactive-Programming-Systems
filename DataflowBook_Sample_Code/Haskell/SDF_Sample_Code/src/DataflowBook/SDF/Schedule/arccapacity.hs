module DataflowBook.SDF.Schedule.ArcCapacity where

import DataflowBook.SDF.Schedule.Index

import Data.Array.IArray

type ArcCapacity = Array ArcIndex Int
    -- 0 -> +inf
