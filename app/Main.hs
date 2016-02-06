{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List
import System.Environment
import System.Process
import System.Exit
import System.IO.Unsafe
import System.Posix.Semaphore
import System.Posix.Signals
import System.Posix.Types
import System.Random

depth :: Int
depth = unsafePerformIO $ maybe 0 read <$> lookupEnv "DEPTH"
{-# NOINLINE depth #-}

indent n = replicate (2 * n) ' '
procId = "[" ++ show depth ++ "]"

printLog s = putStrLn $ indent (depth + 1) ++ procId ++ s

semaphore :: Semaphore
{-# NOINLINE semaphore #-}
semaphore = unsafePerformIO $ semOpen "async_sem0" (OpenSemFlags False False) (CMode 448) 0

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = do
  _ <- semOpen "async_sem0" (OpenSemFlags True True) (CMode 448) n
  printLog $ show n ++ " sems created."

destroyProcessorTokens :: IO ()
destroyProcessorTokens = do
  n <- semGetValue semaphore
  printLog $ show n ++ " sems destroyed."
  semUnlink "async_sem0"

printProcessorTokens :: Semaphore -> IO ()
printProcessorTokens sem = do
  n <- semGetValue sem
  printLog $ "Semaphores = " ++ show n

acquireProcessorToken :: IO ()
acquireProcessorToken = semWait semaphore >> printProcessorTokens semaphore
  -- bracketOnError (semWait semaphore) (const $ semPost semaphore)
  -- (const $ printProcessorTokens semaphore)

releaseProcessorToken :: IO ()
releaseProcessorToken =
  bracketOnError (semPost semaphore) (const $ semWait semaphore)
  (const $ printProcessorTokens semaphore)

withProcessorToken :: IO a -> IO a
withProcessorToken = bracket_ (semWait semaphore) (semPost semaphore)

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken = bracket_ (semPost semaphore) (semWait semaphore)

main :: IO ()
main = bracket_ enter exit $ do
  tid <- myThreadId
  installHandler keyboardSignal (Catch (printLog "catcha0" >> killThread tid)) Nothing
  installHandler killProcess (Catch (printLog "catcha1" >> killThread tid)) Nothing
  installHandler keyboardTermination (Catch (printLog "catcha2" >> killThread tid)) Nothing
  installHandler softwareTermination (Catch (printLog "catcha3" >> killThread tid)) Nothing
  handle (\(e::SomeException) -> printLog ("exception: " ++ show e) >> throwIO e) $ do
    targets <- tail . inits <$> getArgs
    mask $ \restore -> do
      ps <- restore $ parSpawn targets `onException` printLog "exception on parSpawn"
      printLog "waiting.."
      rs <- mapM joinChild ps
      restore . printLog $ "obtain: " ++ show rs

 where
   enter = do
     putStrLn $ indent depth ++ "{"
     when (depth == 0) $ createProcessorTokens 3

   exit = do
     when (depth == 0) destroyProcessorTokens
     putStrLn $ indent depth ++ "}"

   parSpawn = parSpawn' []
   parSpawn' ps (c:cs) = do
     mapM_ checkInterrupted ps
     mask $ \restore -> do
       printLog ("acquireProcessorToken " ++ show c)
       restore $ acquireProcessorToken `onException` printLog "exception on acquireProcessorToken"
       printLog ("spawn " ++ show c)
       p <- restore (spawnChild c) `onException` do {
         printLog $ "exception on spawnChild " ++ show c;
         releaseProcessorToken}
       parSpawn' (p:ps) cs `onException` stopChild p
   parSpawn' ps _ = return $ reverse ps

   -- spawnChild c = asyncBound $ child c
   spawnChild c = asyncBound $ child c `finally` releaseProcessorToken
   stopChild = cancel
   joinChild = waitCatch

   checkInterrupted p = do
     r <- poll p
     case r of
       Just (Left e) -> print ("UserInterrupted", e) >> throwIO e
       _ -> return ()

   child (_:args) = do
     p <- sleepPeriod
     callCommand $ "DEPTH=" ++ show (depth + 1) ++ " sleep " ++ show p ++ " && " ++ "DEPTH=" ++ show (depth + 1) ++ " ./exec.sh " ++ unwords args
   child _ = return ()

   sleepPeriod :: IO Float
   sleepPeriod = getStdRandom (randomR (1,3))
