{-# LANGUAGE FlexibleInstances #-}

module DataflowBook.SDF.Schedule.ActivationVector where

import DataflowBook.SDF.Schedule.Index
import DataflowBook.SDF.Schedule.Pretty

import Data.Array.IArray

type ActivationVector = Array NodeIndex Int
    -- 0 or 1 only

emptyActivationVector :: NodeIndex -> ActivationVector
emptyActivationVector nodeCount = (array (firstIdx,nodeCount) [ (n,0) | n <- [firstIdx..nodeCount]] )

instance PrettyPrint [ActivationVector] where
    prettyPrint vect = concat $ map (\v -> (show v) ++ "\n") vect
