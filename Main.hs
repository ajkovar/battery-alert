import System.Process (readProcess)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Char (isSpace)
import Data.Text (strip, pack, unpack)

strip' :: String -> String
strip' = unpack . strip . pack

readBatteryInfo :: IO String
readBatteryInfo = do
    readProcess "system_profiler" ["SPPowerDataType"] []

prefix :: [Char]
prefix = "State of Charge (%): "

batteryStat = stripPrefix prefix

readBatteryState :: IO (Maybe Float)
readBatteryState = do
    stats <- fmap strip' . lines <$> readBatteryInfo
    let percent = find (isPrefixOf prefix) stats >>= batteryStat
    return $ read <$> percent
