module Types.AluCtrl where

import Clash.Prelude ( Eq, Show, Generic, Default, NFDataX, BitPack )

data AluCtrl
  = AND
  | OR
  | ADD
  | SLL
  | SRL
  | SUB
  | SLT
  | XOR
  | SLTU
  | SRA
  | LUI
  deriving (Show, Eq, Generic, BitPack, NFDataX, Default)
