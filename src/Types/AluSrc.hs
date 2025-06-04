module Types.AluSrc where

import Clash.Prelude

data AluSrc
  = -- | Use Immediate
    IMM
  | -- | Use Execute Bypass
    EX
  | -- | Use Memory bypass
    MEM
  | -- | Use register file
    REG
  deriving (Show, Eq, Generic, BitPack, NFDataX)

instance Default AluSrc where
  def :: AluSrc
  def = REG
