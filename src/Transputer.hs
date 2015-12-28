module Transputer ( Op(..), Transputer ) where

import Data.ByteString
import Data.Int ( Int32 )

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

data TransputerRegisters = TransputerRegisters {
   iptr :: Int32,
   wptr :: Int32,
   areg :: Int32,
   breg :: Int32,
   creg :: Int32,
   oreg :: Int32,
   sreg :: StatusRegisters
} deriving (Show)

data StatusRegisters = StatusRegisters {
  errorFlag :: Bool,
  moveBit :: Bool,
  haltOnErr :: Bool,
  gotoSnp :: Bool,
  ioBit :: Bool,
  timeIns :: Bool,
  timeDel :: Bool,
  distAndIns :: Bool
} deriving (Show)

data Transputer = Transputer {
  registers :: TransputerRegisters,
  memory :: ByteString,
  programEnd :: Int,
  id :: Int
} deriving (Show)
