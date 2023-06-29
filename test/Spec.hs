{-# LANGUAGE BangPatterns #-}

import Spectre
import Spectre.Interactive
import Data.NutMeg

import           Control.Monad               (replicateM)
import           System.Posix.Pty
import           Control.Scheduler           (Comp (..), traverseConcurrently, traverseConcurrently_, replicateConcurrently)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.Map              as M
import           Data.Maybe                  (mapMaybe, fromJust)
import           Text.RawString.QQ
import           Text.Regex.TDFA

main :: IO ()
main = do
    -- filter ((== ALTER) . snd) $ takeWhile ((/= "dcop") . fst) anal
    -- runAll s
    -- writePty (pty s) "(sclListAnalysis)\n" 

    !ss <- traverseConcurrently (ParN n') (\c -> startSession [] (dir' ++ [c] ++ net') (dir' ++ [c])) $ take n ['a' .. ]

    putStrLn $ show n ++ " Parallel Simulations"
    !re <- traverseConcurrently (ParN n') runAll ss
    putStrLn "Done"

    pure ()
  where
    n = 10
    n' = fromIntegral n
    dir' = "/tmp/uhlmanny-"
    net' = "/tb.scs"
