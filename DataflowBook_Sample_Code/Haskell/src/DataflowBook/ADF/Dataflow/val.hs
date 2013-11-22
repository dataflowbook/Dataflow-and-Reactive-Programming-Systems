module DataflowBook.ADF.Dataflow.Val 
(T
,mk ,get
) where

import qualified DataflowBook.ADF.Dataflow.Id as Id


data T t = Val t 
    deriving (Eq,Show)

    
mk = Val

get (Val n) = n 
