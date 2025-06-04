module Types.RegSrc where

import Clash.Prelude (Eq, Generic, NFDataX, Show, Default (def))

data RegSrc
  = -- | PC +4
    PC4
  | -- | data memory
    MEM
  | -- | arithmetic unit
    ALU
  deriving (Show, Eq, Generic, NFDataX)

instance Default RegSrc where
  def :: RegSrc
  def = ALU
