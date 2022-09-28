{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Communication Between Haskell and Spectre
module Spectre.Interactive ( Session (..)
                           , Parameter
                           , initSession
                           , exitSession
                           , runAll
                           , listAnalysis
                           , runAnalysis
                           , parameterValue
                           ) where

import           Spectre
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Char
import qualified Data.Map              as M
import           Data.Maybe                  (fromJust)
import           Data.NutMeg
import           System.Posix.Pty
import           System.IO.Temp
import           System.Directory
import           Text.RawString.QQ
import           Text.Regex.TDFA

-- | Spectre Commands
data Command = Close                -- Quit the Session
             | RunAll               -- Run all simulation analyses
             | ListAnalysis         -- Retrieve Analyses in netlist
             | RunAnalysis Analysis -- Run specified analysis
             | Alter String Float   -- Alter a Parameter
             | GetAttribute String  -- Get Parameter Value

-- | Spectre Interactive Session
data Session = Session { pty :: Pty      -- The Terminal
                       , dir :: FilePath -- Temp Dir
                       }

-- | Name of a netlist Parameter
type Parameter = String

-- | Spectre Interactive mode Prompt
prompt :: BS.ByteString
prompt = "> " :: BS.ByteString

-- | Read Lines from Session Terminal
readLines :: Pty -> IO [BS.ByteString]
readLines pty' = CS.lines <$> readPty pty'

-- | Discard output from session terminal
discardOutput :: Pty -> IO ()
discardOutput pty' = do
    l <- last <$> readLines pty'
    if l == prompt 
       then pure ()
       else discardOutput pty'

-- | Consume all output from Pty
consumeOutput :: Pty -> IO BS.ByteString
consumeOutput pty' = do
    output <- readPty pty'
    if BS.isSuffixOf prompt output 
       then pure output 
       else BS.append output <$> consumeOutput pty'

-- | Initialize spectre session with given include path and netlist
initSession :: [String] -> String -> IO Session
initSession includes netlist = do
    dir' <- createTempDirectory "/tmp" "hspectre"

    let ahdl = dir' ++ "/ahdl"
        raw  = dir' ++ "/hspectre.raw"

    let args = [ "-64", "+interactive"
               , "-format nutbin"
               , "-ahdllibdir " ++ ahdl
               , "-log"
               , "-raw " ++ raw
               ] ++ map ("-I" ++) includes ++ [ netlist ]
    pty' <- fst <$> spawnWithPty Nothing True spectre args (80,100)
    discardOutput pty'
    pure $ Session pty' dir'
  where
    spectre  = "spectre"

-- | Execute a spectre Command which changes the state but returns nothing
exec_ :: Session -> Command -> IO ()
exec_ Session{..} Close                  = writePty pty cmd >> readPty pty >> pure ()
  where
    cmd = "(sclQuit)\n"
exec_ Session{..} RunAll                 = writePty pty cmd >> discardOutput pty
  where
    cmd = "(sclRun \"all\")\n"
exec_ Session{..} (RunAnalysis analysis) = writePty pty cmd >> discardOutput pty
  where
    cmd = CS.pack $ "(sclRunAnalysis (sclGetAnalysis \"" 
       ++ map toLower (show analysis) ++ "\"))\n"
exec_ Session{..} (Alter param value)    = writePty pty cmd >> discardOutput pty
  where
    cmd = CS.pack $ "(sclSetAttribute (sclGetParameter (sclGetCircuit \"\") \"" 
       ++ param ++ "\") \"" ++ show value ++ "\")\n"
exec_ _           _                      = pure ()

-- | Execute spectre command and return some result
exec :: Session -> Command -> IO BS.ByteString
exec Session{..} (GetAttribute param) = fromJust . BS.stripSuffix "\r" 
                                      . last . init . CS.lines 
                                     <$> (writePty pty cmd >> consumeOutput pty)
  where
    cmd = CS.pack 
        $ "(sclGetAttribute (sclGetParameter (sclGetCircuit \"\") \"" 
                ++ param ++ "\") \"value\")\n"
exec Session{..} ListAnalysis         = BS.intercalate "\n" . map parse
                                      . takeWhile (/= ")\r") . drop 1 . CS.lines 
                                     <$> (writePty pty cmd >> consumeOutput pty)
  where
    cmd = "(sclListAnalysis)\n" :: BS.ByteString
    rex = [r|"(.+)" *"(.+)\"|]  :: String
    parse :: BS.ByteString -> BS.ByteString
    parse bs = let (_,_,_,grps) = CS.unpack bs =~ rex :: (String, String, String, [String])
                in CS.pack $ unwords grps
exec _ _                              = pure prompt

-- | Simulation Results
results :: Session -> IO NutMeg
results Session{..} = do
    parseNutMeg <$> readNutRaw raw
  where 
    raw  = dir ++ "/hspectre.raw"

-- | Run all simulation analyses
runAll :: Session -> IO NutMeg
runAll session = exec_ session RunAll >> results session

-- | Get Map of Available Simulation Analyses: (id, type)
listAnalysis :: Session -> IO (M.Map String Analysis)
listAnalysis session = M.map read . M.fromList . filter ((/= "alter") . snd) 
                     . map (asTuple . map CS.unpack . CS.words) . CS.lines 
                    <$> exec session ListAnalysis
  where
    asTuple :: [a] -> (a,a)
    asTuple [a,b] = (a,b)
    asTuple []    = error "Parser Error"
    asTuple _     = error "Parser Error"

-- | Run Selected Analysis only
runAnalysis :: Session -> Analysis -> IO NutMeg
runAnalysis session analysis = exec_ session (RunAnalysis analysis) 
                                    >> results session

-- | Netlist Parameter
parameterValue :: Session -> Parameter -> IO Double
parameterValue session param = read . CS.unpack 
                            <$> exec session (GetAttribute param)

-- | Close a spectre interactive session
exitSession :: Session -> IO ()
exitSession s@Session{..} = do
    exec_ s Close
    closePty pty
    removeDirectoryRecursive dir
