module Types.PcSrc where

import Clash.Prelude (Eq, Generic, NFDataX, Show)

data PcSrc
  = -- | Next PC is PC+4
    PC4
  | -- | Next PC is from jump
    JUMP
  | -- | Next PC is from branch
    BRANCH
  deriving (Show, Eq, Generic, NFDataX)
