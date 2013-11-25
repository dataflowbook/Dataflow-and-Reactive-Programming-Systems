module DataflowBook.SDF.Schedule.Topo where

import DataflowBook.SDF.Schedule.Index

import Data.Array.IArray


type Topo = (Array (NodeIndex,ArcIndex) Int)
    -- -inf -> +inf
