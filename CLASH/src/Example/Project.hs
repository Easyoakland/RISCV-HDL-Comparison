module Example.Project where

import Clash.Prelude

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
topEntity = exposeClockResetEnable accum
-- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "accum",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

-- | A simple accumulator that works on unsigned numbers of any size.
-- It has hidden clock, reset, and enable signals.
accum ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned n)
accum = mealy accumT 0
  where
    accumT s i = (s + i, s)
