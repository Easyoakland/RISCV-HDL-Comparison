module Types.NonImmAluSrc where
import Clash.Prelude

data NonImmAluSrc
  = -- | Use Execute Bypass
    EX
  | -- | Use Memory bypass
    MEM
  | -- | Use register file
    REG
  deriving (Show, Eq, Generic, BitPack, NFDataX)

instance Default NonImmAluSrc where
  def :: NonImmAluSrc
  def = REG
