module TransputerSimulator
    ( printUsage
    ) where

import Transputer

printUsage :: IO ()
printUsage = putStrLn "Transputer Simulator"

step :: Transputer -> Transputer
step t = t
