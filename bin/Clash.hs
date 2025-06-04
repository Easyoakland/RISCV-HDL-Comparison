import Clash.Main (defaultMain)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = getArgs >>= defaultMain . no_prim_warn
  where
    -- Using `Int` makes an unavoidable warning which I can't do anything about which is disabled by passing this argument
    no_prim_warn = ("-fclash-no-prim-warn" :)
