import System.Process (readProcess, runCommand)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Char (isSpace)
import Data.Text (strip, pack, unpack)
import Text.Printf (printf)
import System.Process.Internals (ProcessHandle(ProcessHandle))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))

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

main :: IO (Maybe ProcessHandle)
main = runMaybeT $ do
  level <- MaybeT readBatteryLevel
  MaybeT $ Just <$> runCommand (notifyCommand level)