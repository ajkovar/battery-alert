import System.Process (readProcess, runCommand)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Char (isSpace)
import Data.Text (strip, pack, unpack)
import Text.Printf (printf)
import System.Process.Internals (ProcessHandle(ProcessHandle))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Cont (void)

strip' :: String -> String
strip' = unpack . strip . pack

readBatteryInfo :: IO String
readBatteryInfo = readProcess "system_profiler" ["SPPowerDataType"] []

prefix :: String
prefix = "State of Charge (%): "

readBatteryLevel :: IO (Maybe Float)
readBatteryLevel = do
    stats <- fmap strip' . lines <$> readBatteryInfo
    return $ read <$> (find (isPrefixOf prefix) stats >>= stripPrefix prefix)

notificationBody :: Float -> String
notificationBody = printf "Battery level is at %.0f%%.  Time to unplug!"

notifyCommand :: Float -> String
notifyCommand level =
    printf "osascript -e 'display notification \"%s\" with title \"%s\"'"
           (notificationBody level)
           "Unplug Computer"

isOutsideTargetRange :: Float -> Bool
isOutsideTargetRange = (||) <$> (>80) <*> (<70)

notifyIfLow :: IO ()
notifyIfLow = do
  level <- readBatteryLevel
  maybe (return ()) (void . runCommand . notifyCommand) level 

sleepDuration :: Int
sleepDuration = 1000000 * 30

main :: IO ()
main = do
  notifyIfLow
  threadDelay sleepDuration
  main