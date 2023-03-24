-- just messing around with Haskell IO for fun and practice

{-# LANGUAGE TupleSections, LambdaCase #-}

import Control.Applicative ( liftA2 )
import Control.Arrow
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad.Loops
import Control.Monad.State as S
import Control.Monad.Trans.State as T
import Data.Function ( fix, (&) )
import System.Clock ( Clock(Realtime), getTime, sec, TimeSpec )
import System.IO ( BufferMode(NoBuffering), hSetBuffering, hSetEcho, stdin )
import Text.Read ( readMaybe )

main :: IO ()
--main = evalStateT printStateT $ Range 300 42
--main = evalStateT (preModify printStateT) $ Range 300 42
--main = evalStateT seekDo $ Range 0 100
--main = hSetBuffering stdin NoBuffering >>
--       hSetEcho stdin False >>
--       evalStateT ropeWalk (ropeLength `div` 2)
--main = hSetBuffering stdin NoBuffering >>
--       hSetEcho stdin False >>
--       loopStateIO' ropeWalk' (ropeLength `div` 2)
--main = loopStateIO tickTickBoom (TimeSpec 0 0, -1)
--main = loopStateIO' tickTickBoom (TimeSpec 0 0, -1)
main = tickTickBoom' 0

guess :: Int -> IO ()
guess n = fmap readMaybe ask >>= check
    where ask = putStrLn "Take a guess!" >> getLine
          check Nothing  = putStrLn "Sorry, not a number..." >> guess n
          check (Just x) = if x == n then putStrLn "Congratulations!"
                                     else guess n

-- this is equivalent to the above
guess' :: Int -> IO ()
guess' n = do
    putStrLn "Take a guess!"
    raw <- getLine
    let x = readMaybe raw
    case x of Nothing -> putStrLn "Sorry, not a number..." >> guess n
              Just y  -> if y == n then putStrLn "Congratulations!"
                                    else guess n

seek :: Int -> Int -> IO ()
seek bottom top = prompt >> getLine >>= react
    where prompt = putStrLn $ "Higher than " <> show newGuess <> "? (y/n/!)"
          newGuess = floor $ fromIntegral (bottom + top) / 2
          react (c:_) = case c of '!' -> putStrLn "Yay!"
                                  'y' -> seek (newGuess + 1) top
                                  _   -> seek bottom (newGuess - 1)


-- let's try to do State and IO at the same time, see how it goes

data Range = Range {
    bottom :: Int,
    top    :: Int
} deriving (Eq, Show)

bimap :: (Int -> Int) -> (Int -> Int) -> Range -> Range
bimap f g r = Range { bottom = f . bottom $ r,
                      top    = g . top    $ r }

printState :: State Range (IO ())
printState = putStrLn . show <$> S.get

-- lift is to MonadTrans what return is to Monad, I think
printStateT :: StateT Range IO ()
printStateT = T.get >>= lift . putStrLn . show

printStateTButModifyFirst :: StateT Range IO ()
printStateTButModifyFirst = (T.modify $ bimap (+1) (*2)) >> T.get >>= lift . putStrLn . show

preModify :: StateT Range IO () -> StateT Range IO ()
preModify = withStateT $ bimap (*2) (`div` 2)

-- okay this is a bit cumbersome, I think I see the reason for do-notation
seek' :: StateT Range IO ()
seek' = T.get >>= \r -> liftIO (prompt r) >> liftIO getLine >>= updateRange r
    where prompt r = putStrLn $ "Higher than " <> show (newGuess r) <> "? (y/n/!)"
          newGuess r = floor $ fromIntegral (bottom r + top r) / 2
          updateRange r (c:_) = case c of '!' -> liftIO $ putStrLn "Yay!"
                                          'y' -> (T.put $ Range (newGuess r + 1) (top r)) >> seek'
                                          _   -> (T.put $ Range (bottom r) (newGuess r - 1)) >> seek'

seekDo :: StateT Range IO ()
seekDo = do
    r <- T.get
    let min = bottom r
    let max = top r
    let newGuess = floor $ fromIntegral (min + max) / 2
    liftIO . putStrLn $ "Higher than " <> show newGuess <> "? (y/n/!)"
    (c:_) <- liftIO getLine
    case c of '!' -> liftIO $ putStrLn "Yay!"
              'y' -> (T.put $ Range (newGuess + 1) max) >> seekDo
              _   -> (T.put $ Range min (newGuess - 1)) >> seekDo

ropeLength :: Int
ropeLength = 9

ropeWalk :: StateT Int IO ()
ropeWalk = do
    n <- T.get
    let tilesLeft  = n
    let tilesRight = ropeLength - n - 1
    liftIO . putStr . take tilesLeft $ repeat '_'
    liftIO . putStr $ "#"
    liftIO . putStrLn . take tilesRight $ repeat '_'
    key <- liftIO getChar
    when (key == 'C') $ T.modify (+1)
    when (key == 'D') $ T.modify (+(-1))
    n <- T.get
    if n < 0 || n >= ropeLength
        then liftIO . putStrLn $ "You fell off the rope."
        else unless (key == 'q' || key == 'Q') ropeWalk


-- let's refactor this a little bit...
loopStateIO :: StateT s IO Bool -> s -> IO ()
loopStateIO f s = runStateT f s >>= \case (True, s') -> loopStateIO f s'
                                          (_   , s') -> pure ()

-- hey, look at that, even cleaner!
loopStateIO' :: Monad m => StateT s m Bool -> s -> m ()
loopStateIO' = fmap void . runStateT . iterateUntil not

showRope :: Int -> IO ()
showRope n = putStr $ "\r" <> replicate tilesLeft '_' <> "#" <> replicate tilesRight '_'
    where tilesLeft  = n
          tilesRight = ropeLength - n - 1

ropeWalk' :: StateT Int IO Bool
ropeWalk' = do
    n <- T.get
    liftIO $ showRope n
    key <- liftIO getChar
    when (key == 'C') $ T.modify (+1)
    when (key == 'D') $ T.modify (+(-1))
    n <- T.get
    when (n < 0 || n >= ropeLength) . liftIO . putStrLn $ "\nYou fell off the rope."
    if key == 'q' || key == 'Q' || n < 0 || n >= ropeLength
        then pure False
        else pure True


tickTickBoom :: StateT (TimeSpec, Int) IO Bool
tickTickBoom = do
    (time, n) <- T.get
    time' <- liftIO $ getTime Realtime
    when (sec time < sec time') (T.modify $ second (+1))
    liftIO $ showRope n
    T.modify . first . const $ time'
    if n >= ropeLength then (liftIO . putStrLn $ "\nBoom!") >> pure False
                       else pure True

tickTickBoom' :: Int -> IO ()
tickTickBoom' n = if n >= ropeLength then putStrLn $ "\nBoom!"
                                     else (showRope n >>) $ void . forkIO $
                                          threadDelay 1000000 >> tickTickBoom' (succ n)
