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
import System.Posix.Types

depth :: Int
depth = unsafePerformIO $ maybe 0 read <$> lookupEnv "DEPTH"
{-# NOINLINE depth #-}

indent n = replicate (2 * n) ' '
procId = "[" ++ show depth ++ "]"

printLog s = putStrLn $ indent (depth + 1) ++ procId ++ s

semaphore :: Semaphore
{-# NOINLINE semaphore #-}
semaphore = unsafePerformIO $ semOpen "async_sem" (OpenSemFlags False False) (CMode 448) 0

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = do
  _ <- semOpen "async_sem" (OpenSemFlags True True) (CMode 448) n
  printLog $ show n ++ " sems created."

destroyProcessorTokens :: IO ()
destroyProcessorTokens = do
  n <- semGetValue semaphore
  printLog $ show n ++ " sems destroyed."
  semUnlink "async_sem"

printProcessorTokens :: Semaphore -> IO ()
printProcessorTokens sem = do
  n <- semGetValue sem
  printLog $ "Semaphores = " ++ show n

acquireProcessorToken :: IO ()
acquireProcessorToken =
  bracketOnError (semWait semaphore) (const $ semPost semaphore)
  (const $ printProcessorTokens semaphore)

releaseProcessorToken :: IO ()
releaseProcessorToken =
  bracketOnError (semPost semaphore) (const $ semWait semaphore)
  (const $ printProcessorTokens semaphore)

withProcessorToken :: IO a -> IO a
withProcessorToken = bracket_ (semWait semaphore) (semPost semaphore)

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken = bracket_ (semPost semaphore) (semWait semaphore)

main :: IO ()
main = bracket_ enter exit $
  handle (\(e::SomeException) -> printLog ("exception: " ++ show e) >> throwIO e) $ do
    targets <- getArgs
    ps <- parSpawn . tail $ inits targets
    mask_ (printLog "waiting.." >> mapM joinChild ps >>= printLog . ("obtain: " ++ ) . show)

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
     acquireProcessorToken
     printLog $ "spawn " ++ show c
     p <- spawnChild c
     parSpawn' (p:ps) cs `onException` stopChild p
   parSpawn' ps _ = return $ reverse ps

   spawnChild c = asyncBound $ child c `finally` releaseProcessorToken
   stopChild = cancel
   joinChild = waitCatch

   checkInterrupted p = do
     r <- poll p
     case r of
       Just (Left e) -> print ("UserInterrupted", e) >> throwIO e
       _ -> return ()

   child (_:args) = callCommand $ "DEPTH=" ++ show (depth + 1) ++ " sleep 2 && " ++ "DEPTH=" ++ show (depth + 1) ++ " ./exec.sh " ++ unwords args
   child _ = return ()
