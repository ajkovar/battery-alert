import System.Process (readProcess, runCommand)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Text (strip, pack, unpack)
import Text.Printf (printf)
import System.Process.Internals (ProcessHandle(ProcessHandle))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Cont (void)
import Data.Char (toUpper)

strip' :: String -> String
strip' = unpack . strip . pack

readBatteryInfo :: IO String
readBatteryInfo = readProcess "system_profiler" ["SPPowerDataType"] []

readBatteryStat :: String -> IO (Maybe String)
readBatteryStat prefix = do
    stats <- fmap strip' . lines <$> readBatteryInfo
    return $ find (isPrefixOf prefix) stats >>= stripPrefix prefix

readBatteryLevel :: IO (Maybe Float)
readBatteryLevel = fmap read <$> readBatteryStat "State of Charge (%): "

readBatteryChargingStatus :: IO (Maybe Bool)
readBatteryChargingStatus = fmap (=="Yes") <$> readBatteryStat "Charging: "

notificationBody :: Float -> String -> String
notificationBody = printf "Battery level is at %.0f%%.  Time to %s!"

notificationHeader :: String -> String
notificationHeader (head:tail) = printf "%s Computer" (toUpper head:tail)

notifyCommand :: Float -> String -> String
notifyCommand level =
    printf "osascript -e 'display notification \"%s\" with title \"%s\"'" <$>
           notificationBody level <*>
           notificationHeader

notifyIfLow :: IO (Maybe ())
notifyIfLow = runMaybeT $ do
  level <- MaybeT readBatteryLevel
  isCharging <- MaybeT readBatteryChargingStatus
  if level > 60 && isCharging then
    notify level "unplug"
  else when (level < 40 && not isCharging) $
    notify level "plug"
  where notify level action = MaybeT $ void . Just <$> runCommand (notifyCommand level action)

sleepDuration :: Int
sleepDuration = 1000000 * 60 * 5

main :: IO ()
main = do
  notifyIfLow
  threadDelay sleepDuration
  main