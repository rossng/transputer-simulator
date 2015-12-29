module Transputer ( Op(..), Transputer(..), Registers(..), StatusRegisters(..) ) where

import Data.ByteString
import Data.Int ( Int32 )
import Control.Lens

data Op = Prefix
        | NegativePrefix
        | Operate
        | LoadConstant
        | LoadLocal
        | StoreLocal
        | LoadLocalPointer
        | AddConstant
        | EqualsConstant
        | Jump
        | ConditionalJump
        | LoadNonLocal
        | StoreNonLocal
        | LoadNonLocalPointer
        | Call
        | AdjustWorkspace
        | Reverse
        | Add
        | Subtract
        | Multiply
        | Divide
        | Remainder
        | Sum
        | Difference
        | Product
        | And
        | Or
        | ExclusiveOr
        | BitwiseNot
        | ShiftLeft
        | ShiftRight
        | GreaterThan
        | LoopEnd
        | ByteCount
        | WordCount
        | LoadPointerToInstruction
        | MinimumInteger
        | ByteSubscript
        | WordSubscript
        | MoveMessage
        | InputMessage
        | OutputMessage
        | LoadByte
        | StoreByte
        | OutputByte
        | OutputWord
        | GeneralCall
        | GeneralAdjustWorkspace
        | Return
        | StartProcess
        | EndProcess
        | RunProcess
        | StopProcess
        | LoadCurrentPriority
        | LoadTimer
        | TimerInput
        | AltStart
        | AltWait
        | AltEnd
        | TimerAltStart
        | TimerAltWait
        | EnableSkip
        | DisableSkip
        | EnableChannel
        | DisableChannel
        | EnableTimer
        | DisableTimer
        | CheckSubscriptFromZero
        | CheckCountFromOne
        | TestErrorFalseAndClear
        | StopOnError
        | SetError
        | ExtendToWord
        | CheckWord
        | ExtendToDouble
        | CheckSingle
        | LongAdd
        | LongSubtract
        | LongSum
        | LongDiff
        | LongMultiply
        | LongDivide
        | LongShiftLeft
        | LongShiftRight
        | Normalise
        | ResetChannel
        | TestProcessorAnalysing
        | StoreHighPriorityFrontPointer
        | StoreLowPriorityFrontPointer
        | StoreTimer
        | StoreHighPriorityBackPointer
        | StoreLowPriorityBackPointer
        | SaveHighPriorityQueueRegisters
        | SaveLowPriorityQueueRegisters
        | ClearHaltOnError
        | SetHaltOnError
        | TestHaltOnError
        | FractionalMultiply
        | UnpackSingleLengthFpNumber
        | RoundSingleLengthFpNumber
        | PostNormaliseCorrectionOfSingleLengthFpNumber
        | LoadSingleLengthInfinity
        | CheckSingleLengthFpInfinityOrNaN
        | DuplicateTopOfStack
        | InitialiseDataForTwoDimensionalBlockMove
        | TwoDimensionalBlockCopy
        | TwoDimensionalBlockCopyNonZeroBytes
        | TwoDimensionalBlockCopyZeroBytes
        | CalculateCRCOnWord
        | CalculateCRCOnByte
        | CountBitsSetInWord
        | ReverseBitsInWord
        | ReverseBottomNBitsInWord
        | FormDoubleWordSubscript
    deriving (Show)

data Registers = Registers {
   _iptr :: Int32,
   _wptr :: Int32,
   _areg :: Int32,
   _breg :: Int32,
   _creg :: Int32,
   _oreg :: Int32,
   _sreg :: StatusRegisters
} deriving (Show)

data StatusRegisters = StatusRegisters {
  _errorFlag :: Bool,
  _moveBit :: Bool,
  _haltOnErr :: Bool,
  _gotoSnp :: Bool,
  _ioBit :: Bool,
  _timeIns :: Bool,
  _timeDel :: Bool,
  _distAndIns :: Bool
} deriving (Show)

data Transputer = Transputer {
  _registers :: Registers,
  _memory :: ByteString,
  _programEnd :: Int,
  _id :: Int
} deriving (Show)
