{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Communication Between Haskell and Spectre
module Spectre.Interactive ( Session (..)
                           , Parameter
                           , startSession
                           , startSession'
                           , stopSession
                           , runAll
                           , listAnalysis
                           , runAnalysis
                           , getParameter
                           , setParameter
                           , getParameters
                           , setParameters
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
data Command = Close                      -- Quit the Session
             | RunAll                     -- Run all simulation analyses
             | ListAnalysis               -- Retrieve Analyses in netlist
             | RunAnalysis Analysis       -- Run specified analysis
             | SetAttribute String Double -- Alter a Parameter
             | GetAttribute String        -- Get Parameter Value

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

-- | Write offset to file
writeOffset :: Session -> Int -> IO ()
writeOffset Session{..} offset = BS.writeFile (dir ++ "/offset") . CS.pack 
                               $ show offset

-- | Read offset
readOffset :: Session -> IO Int
readOffset Session{..} = read . CS.unpack <$> BS.readFile (dir ++ "/offset")

-- | Initialize spectre session with given include path and netlist
startSession' :: [FilePath] -> FilePath -> IO Session
startSession' includes netlist = createTempDirectory "/tmp" "hspectre" 
                                    >>= startSession includes netlist

-- | Initialize spectre session with given include path, netlist and temp dir
startSession :: [FilePath] -> FilePath -> FilePath -> IO Session
startSession inc net dir' = do
    let ahdl = dir' ++ "/ahdl"
        raw  = dir' ++ "/hspectre.raw"

    let args = [ "-64", "+interactive"
               , "-format nutbin"
               , "-ahdllibdir " ++ ahdl
               , "+multithread"
               , "-log"
               , "-raw " ++ raw
               ] ++ map ("-I" ++) inc ++ [ net ]
    pty' <- fst <$> spawnWithPty Nothing True spectre args (80,100)
    discardOutput pty'

    let session = Session pty' dir'
    writeOffset session 0

    pure session
  where
    spectre  = "spectre"

-- | Execute a spectre Command which changes the state but returns nothing
exec_ :: Session -> Command -> IO ()
exec_ Session{..} Close                      = writePty pty cmd >> readPty pty >> pure ()
  where
    cmd = "(sclQuit)\n"
exec_ Session{..} RunAll                     = writePty pty cmd >> discardOutput pty
  where
    cmd = "(sclRun \"all\")\n"
exec_ Session{..} (RunAnalysis analysis)     = writePty pty cmd >> discardOutput pty
  where
    cmd = CS.pack $ "(sclRunAnalysis (sclGetAnalysis \"" 
       ++ map toLower (show analysis) ++ "\"))\n"
exec_ Session{..} (SetAttribute param value) = writePty pty cmd >> discardOutput pty
  where
    cmd = CS.pack 
        $ "(sclGetAttribute (sclGetParameter (sclGetCircuit \"\") \"" 
                ++ param ++ "\") \"value\" " ++ show value ++ ")\n"
exec_ _           _                          = pure ()

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
results s@Session{..} = do
    off' <- readOffset s
    (nut, off) <- parseNutMeg' off' <$> readNutRaw (dir ++ "/hspectre.raw")
    writeOffset s off
    pure nut

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

-- | Get Netlist Parameter
getParameter :: Session -> Parameter -> IO Double
getParameter session param = read . CS.unpack 
                          <$> exec session (GetAttribute param)

-- | Get a list of Parameters as Map
getParameters :: Session -> [Parameter] -> IO (M.Map Parameter Double)
getParameters session params = do
    values <- mapM (getParameter session) params
    pure . M.fromList $ zip params values

-- | Set Netlist Parameter
setParameter :: Session -> Parameter -> Double -> IO ()
setParameter session param value = exec_ session (SetAttribute param value)

-- | Get a list of Parameters
setParameters :: Session -> M.Map Parameter Double -> IO (M.Map Parameter ())
setParameters session = M.traverseWithKey (setParameter session) 

-- | Close a spectre interactive session
stopSession :: Session -> IO ()
stopSession s@Session{..} = do
    exec_ s Close
    closePty pty
    removeDirectoryRecursive dir
