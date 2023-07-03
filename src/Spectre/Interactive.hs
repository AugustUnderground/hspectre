{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Interactive Communication between Haskell and Spectre
module Spectre.Interactive ( -- * Types
                             Session (..), Parameter
                           -- * Session Management
                           , startSession, startSession', stopSession
                           -- * Running Simulations
                           , runAll, runAll_, results, results'
                           , listAnalysis, runAnalysis, sweep
                           -- * Netlist Parameters
                           , getParameter, setParameter
                           , getParameters, setParameters
                           ) where

import           Spectre
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Char
import qualified Data.Map              as M
import           Data.Maybe                  (fromJust)
import           Data.NutMeg                 (NutMeg)
import qualified Data.NutMeg           as N
import           Control.Monad               (when)
import           System.Process
import           System.Posix.Pty
import           System.IO.Temp
import           System.Directory
import           Text.RawString.QQ
import           Text.Regex.TDFA

-- | Spectre Commands
data Command = Close                        -- ^ Quit the Session
             | RunAll                       -- ^ Run all simulation analyses
             | ListAnalysis                 -- ^ Retrieve Analyses in netlist
             | RunAnalysis  !String         -- ^ Run specified analysis
             | SetAttribute !String !Double -- ^ Alter a Parameter
             | GetAttribute !String         -- ^ Get Parameter Value

instance Show Command where
  show Close              = "(sclQuit)"
  show RunAll             = "(sclRun \"all\")"
  show ListAnalysis       = "(sclListAnalysis)"
  show (RunAnalysis  a)   = "(sclRunAnalysis (sclGetAnalysis " ++ show (map toLower a) ++ "))"
  show (SetAttribute a v) = "(sclSetAttribute (sclGetParameter (sclGetCircuit \"\") "
                                ++ show a ++ ") \"value\" " ++ show v ++ ")"
  show (GetAttribute a)   = "(sclGetAttribute (sclGetParameter (sclGetCircuit \"\") "
                                ++ show a ++ ") \"value\")"
-- | Write command
writeCommand :: Command -> BS.ByteString
writeCommand cmd = CS.pack $ show cmd ++ "\n"

-- | Spectre Interactive Session
data Session = Session { pty    :: !Pty      -- ^ The pseudo terminal
                       , dir    :: !FilePath -- ^ Simulation data directory
                       }

-- | Name of a netlist Parameter
type Parameter = String

-- | Spectre Interactive mode Prompt
prompt :: BS.ByteString
prompt = "> " :: BS.ByteString

-- | Consume all output from Pty
consumeOutput :: Pty -> IO BS.ByteString
consumeOutput pty' = do
    !output <- drainOutput pty' >> readPty pty'
    if BS.isSuffixOf prompt output 
       then pure output 
       else BS.append output <$> consumeOutput pty'

-- | Discard output from session terminal
discardOutput :: Pty -> IO ()
discardOutput pty' = do
    !_ <- consumeOutput pty' 
    pure ()

-- | Initialize spectre session with given include path and netlist
startSession' :: [FilePath] -> FilePath -> IO Session
startSession' includes netlist = createTempDirectory "/tmp" "hspectre" 
                                    >>= startSession includes netlist

-- | Initialize spectre session with given include path, netlist and temp dir
startSession :: [FilePath] -> FilePath -> FilePath -> IO Session
startSession inc net dir' = do
    doesFileExist log' >>= flip when (removeFile log')
    createDirectoryIfMissing True dir'

    !_ <- spawnCommand $! "mkfifo " ++ log'
    pty' <- fst <$> spawnWithPty Nothing True spectre args (80,100)
    !_ <- spawnCommand $! "cat "    ++ log' ++ " > /dev/null &"

    let session = Session pty' dir'

    !_ <- threadWaitReadPty pty' >> consumeOutput pty'

    pure session
  where
    spectre = "spectre"
    ahdl    = dir' ++ "/ahdl"
    raw     = dir' ++ "/hspectre.raw"
    log'    = dir' ++ "/hspectre.log"
    args    = [ "-64", "+interactive"
              , "-format nutbin"
              , "-ahdllibdir " ++ ahdl
              , "+multithread"
              , "=log " ++ log'
              , "-raw " ++ raw
              ] ++ map ("-I" ++) inc ++ [ net ]

-- | Execute a spectre Command which changes the state but returns nothing
exec_ :: Session -> Command -> IO ()
exec_ Session{..} Close = writePty pty (writeCommand Close) >> readPty pty >> pure ()
exec_ Session{..} cmd   = writePty pty (writeCommand cmd)   >> discardOutput pty

-- | Execute spectre command and return some result
exec :: Session -> Command -> IO BS.ByteString
exec Session{..} cmd@(GetAttribute _) = fromJust . BS.stripSuffix "\r" 
                                      . last . init . CS.lines 
                                     <$> (writePty pty cmd' >> consumeOutput pty)
  where
    cmd' = writeCommand cmd
exec Session{..} ListAnalysis         = BS.intercalate "\n" . map parse
                                      . takeWhile (/= ")\r") . drop 1 . CS.lines 
                                     <$> ( writePty pty cmd >> consumeOutput pty)
  where
    cmd = writeCommand ListAnalysis
    rex = [r|"(.+)" *"(.+)\"|]  :: String
    parse :: BS.ByteString -> BS.ByteString
    parse bs = let (_,_,_,grps) = CS.unpack bs =~ rex :: (String, String, String, [String])
                in CS.pack $ unwords grps
exec Session{..} cmd                  = writePty pty (writeCommand cmd) >> consumeOutput pty

-- | Get Simulation Results (Lazy)
results :: Session -> IO NutMeg
results Session{..} = N.readFile (dir ++ "/hspectre.raw")

-- | Get Simulation Results (Strict)
results' :: Session -> IO NutMeg
results' Session{..} = N.readFile' (dir ++ "/hspectre.raw")

-- | Run all simulation analyses
runAll :: Session -> IO NutMeg
runAll session = exec_ session RunAll >> results session

-- | Run all simulation analyses don't read results
runAll_ :: Session -> IO ()
runAll_ session = exec_ session RunAll 

-- | Get Map of Available Simulation Analyses: (id, type)
listAnalysis :: Session -> IO [(String, Analysis)]
listAnalysis session = map (snd' read . asTuple . map CS.unpack . CS.words)
                     . CS.lines <$> exec session ListAnalysis
  where
    snd' f (a,b) = (a, f b)
    asTuple :: [a] -> (a,a)
    asTuple [a,b] = (a,b)
    asTuple _     = error "Parser Error"

-- | Run Selected Analysis only
runAnalysis :: Session -> String -> IO NutMeg
runAnalysis session analysisID = exec_ session (RunAnalysis analysisID) 
                                        >> results session

-- | Get Netlist Parameter
getParameter :: Session -> Parameter -> IO Double
getParameter session param = read . CS.unpack 
                          <$> exec session (GetAttribute param)

-- | Get a list of Parameters as Map
getParameters :: Session -> [Parameter] -> IO (M.Map Parameter Double)
getParameters session params = do
    !values <- mapM (getParameter session) params
    pure . M.fromList $ zip params values

-- | Set Netlist Parameter
setParameter :: Session -> Parameter -> Double -> IO ()
setParameter session param value = exec_ session (SetAttribute param value)

-- | Get a list of Parameters
setParameters :: Session -> M.Map Parameter Double -> IO (M.Map Parameter ())
setParameters session = M.traverseWithKey (setParameter session) 

-- | Perform a number of simulation analyses for a given list of parameter maps
-- and read the results only afterwards.
sweep :: Session -> [M.Map Parameter Double] -> IO NutMeg
sweep session params = mapM_ (\p -> setParameters session p >> runAll_ session) params
                        >> results' session

-- | Close a spectre interactive session
stopSession :: Session -> IO ()
stopSession s@Session{..} = do
    exec_ s Close
    closePty pty
    removeDirectoryRecursive dir
