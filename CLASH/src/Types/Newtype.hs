module Types.Newtype where

import Clash.Prelude (BitPack, BitVector, Bool (..), Eq, Generic, NFDataX, Show, Signed, Unsigned, Default (..))
-- import Data.Default.Internal ()

newtype Reg = Reg (Unsigned 5) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Word = Word (Signed 32) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Instruction = Instruction (BitVector 32) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Ptr = Ptr (Unsigned 32) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Opcode = Opcode (Unsigned 7) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Funct7 = Funct7 (Unsigned 7) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Funct3 = Funct3 (Unsigned 3) deriving (Show, Eq, Generic, BitPack, NFDataX, Default)

newtype Enable = Enable Bool deriving (Show, Eq, Generic, BitPack, NFDataX, Default)
