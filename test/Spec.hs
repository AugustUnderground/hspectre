{-# LANGUAGE BangPatterns #-}

import Spectre
import Spectre.Interactive
import qualified Data.NutMeg as N

import           Control.Monad               (replicateM, replicateM_)
import           System.Posix.Pty
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.Map              as M
import qualified Data.Vector.Unboxed as V
import           Data.Complex
import           Data.Maybe                  (mapMaybe, fromJust)
import           Text.RawString.QQ
import           Text.Regex.TDFA

main :: IO ()
main = do
    -- filter ((== ALTER) . snd) $ takeWhile ((/= "dcop") . fst) anal
    -- runAll s
    -- writePty (pty s) "(sclListAnalysis)\n" 

    !s <- startSession [] (dir' ++ net') dir'
    -- replicateM_ 130 (runAll_ s)

    !nut <- sweep s (replicate 100 M.empty)
    let ac = M.map N.asVector $! snd (nut !! 7) :: M.Map String (V.Vector (Complex Double))
    -- !nut <- N.readFile' (dir' ++ "/hspectre.raw")

    pure ()
  where
    n = 10
    n' = fromIntegral n
    dir' = "/home/uhlmanny/Workspace/hspectre/__app"
    net' = "/tb.scs"
