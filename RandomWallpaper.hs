-- RandomWallpaper.hs

-- Picks and sets an image from ~/Pictures/wallpapers/ as desktop wallpaper.
-- Say `crontab -e' and use `runhaskell' to set a cron job to run this every
-- once in a while.

-- You need to have `feh' installed for this script to work. It comes packaged
-- on several common distributions such as Debian, Arch and Ubuntu.
-- Note that you also need to tell `feh' your X display's identifier. The
-- cron job I'm using looks like this:
-- export DISPLAY=":0" ; runhaskell $HOME/minihacks/RandomWallpaper.hs

import Control.Applicative ( (<$>) )
import Data.Composition    ( (.:) )

import System.Directory    ( getHomeDirectory, getDirectoryContents )
import System.Process      ( callCommand )
import System.Random       ( StdGen, getStdGen, randomR )

wallpapersDir :: IO FilePath
wallpapersDir = fmap (++ "/Pictures/wallpapers/") getHomeDirectory

pickRandom :: StdGen -> [a] -> a
pickRandom g xs = xs !! (fst .: randomR) (0, length xs - 1) g

setWallpaper :: FilePath -> IO ()
setWallpaper = callCommand . ("feh --bg-fill " ++)

main = do
    g  <- getStdGen
    wd <- wallpapersDir
    ws <- fmap (wd ++) <$> getDirectoryContents wd
    setWallpaper $ pickRandom g ws

