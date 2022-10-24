{-# OPTIONS_GHC -Wall #-}

-- | Communication Between Haskell and Spectre
module Spectre ( Analysis (..)
               , version
               , simulate
               ) where

import Data.NutMeg
import System.Command
import System.IO.Temp

-- | Available Simulation Analyses
data Analysis = AC      -- ^ AC Analysis
              | DC      -- ^ DC Analysis
              | DCMATCH -- ^ Offset Analysis
              | STB     -- ^ Stability Analysis
              | TRAN    -- ^ Transient Analysis
              | NOISE   -- ^ Noise Analysis
              | XF      -- ^ XF Analysis
              deriving (Eq)

-- | Show instance for analyses are lowercase
instance Show Analysis where
  show AC      = "ac"
  show DC      = "dc"
  show DCMATCH = "dcmatch"
  show STB     = "stb"
  show TRAN    = "tran"
  show NOISE   = "noise"
  show XF      = "xf"

-- | Read instance for analyses handle lowercase
instance Read Analysis where
  readsPrec _ "ac"      = [(AC, "")]
  readsPrec _ "dc"      = [(DC, "")]
  readsPrec _ "dcmatch" = [(DCMATCH, "")]
  readsPrec _ "stb"     = [(STB, "")]
  readsPrec _ "tran"    = [(TRAN, "")]
  readsPrec _ "noise"   = [(NOISE, "")]
  readsPrec _ "xf"      = [(XF, "")]
  readsPrec _ _         = undefined

-- | Get Spectre Version
version :: IO String
version = do
    Stdout v <- command [] "spectre" ["-V"]
    pure v

-- | Simulate a netlist non interactively and return simulation results
simulate :: [String] -> String -> IO NutMeg
simulate includes netlist = do
    tmp <- createTempDirectory "/tmp" "hspectre"
    let raw' = tmp ++ "/hspectre.raw"
        log' = tmp ++ "/hspectre.log"
    let args = [ "-64", "-format nutbin", "-raw " ++ raw', "=log " ++ log' 
               ] ++ incs ++ [netlist]
    command_ [] spectre args
    parseNutMeg <$> readNutRaw raw'
  where
    spectre = "spectre"
    incs    = map ("-I"++) includes
