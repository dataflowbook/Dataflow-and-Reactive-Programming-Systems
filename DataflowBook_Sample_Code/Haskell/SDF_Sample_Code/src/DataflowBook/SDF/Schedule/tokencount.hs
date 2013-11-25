module DataflowBook.SDF.Schedule.TokenCount where

import DataflowBook.SDF.Schedule.Index

import Data.Array.IArray

type TokenCount = Array ArcIndex Int    -- 0 -> +inf


emptyTokenCount :: ArcIndex -> TokenCount
emptyTokenCount arcCount = array (firstIdx,arcCount)
    [ (a, 0)
    | a <- [firstIdx .. arcCount]
    ]


