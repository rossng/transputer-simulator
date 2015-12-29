{-# LANGUAGE TemplateHaskell #-}

module TransputerSimulator ( printUsage, step ) where

import Control.Lens
import Transputer

printUsage :: IO ()
printUsage = putStrLn "Transputer Simulator"

makeLenses ''Transputer
makeLenses ''StatusRegisters
makeLenses ''Registers

step :: Transputer -> Transputer
step = over (registers . iptr) (+ 1)
