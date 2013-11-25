module DataflowBook.SDF.Schedule.ActivationMatrix where

import DataflowBook.SDF.Schedule.Index

import Data.Array.IArray

type ActivationMatrix = Array (NodeIndex,ArcIndex) Int
    -- 0 or 1 only
