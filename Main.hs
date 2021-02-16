import System.Process (readProcess)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Char (isSpace)
import Data.Text (strip, pack, unpack)

strip' :: String -> String
strip' = unpack . strip . pack

readBatteryInfo :: IO String
readBatteryInfo = readProcess "system_profiler" ["SPPowerDataType"] []

prefix :: String
prefix = "State of Charge (%): "

readBatteryState :: IO (Maybe Float)
readBatteryState = do
    stats <- fmap strip' . lines <$> readBatteryInfo
    return $ read <$> (find (isPrefixOf prefix) stats >>= stripPrefix prefix)