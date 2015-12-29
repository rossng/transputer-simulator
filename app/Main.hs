module Main where

import TransputerSimulator as TS
import Transputer as T
import qualified Data.ByteString.Char8 as BC

transputer :: Transputer
transputer = T.Transputer {
                _registers = T.Registers {
                  _iptr = 0, _wptr = 0, _areg = 0, _breg = 0, _creg = 0, _oreg = 0,
                  _sreg = T.StatusRegisters {
                    _errorFlag = False,
                    _moveBit = False,
                    _haltOnErr = False,
                    _gotoSnp = False,
                    _ioBit = False,
                    _timeIns = False,
                    _timeDel = False,
                    _distAndIns = False
                  }
                },
                _memory = BC.pack "",
                _programEnd = 5,
                _id = 1
              }

main :: IO ()
main = do
        TS.printUsage
        putStrLn $ show transputer
        putStrLn $ show (TS.step transputer)
