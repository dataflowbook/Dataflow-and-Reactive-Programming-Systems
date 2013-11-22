--module DataflowBook.ADFTest where
module Main where

import DataflowBook.ADF.TestApps
import DataflowBook.ADF.Manchester.Engine

-- | Main entry point of test program
-- | Use one of the test apps in TestApps.hs
-- | or create you own
main = do
    runMT app_powers_of_n

