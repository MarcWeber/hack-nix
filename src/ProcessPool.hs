{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module ProcessPool where
import Control.Concurrent
import Data.Function
import Control.Monad
import Control.Exception.Base
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
{- unfortunately there is no chance reusing an existing library because we have to create real OS threads -}

data TaskStateChanged = TSCStarted | TSCStopped | TSCTaskAdded


printNoProgress :: (Show label)
                 => (Ord tt)
                 => TaskStateChanged 
                    -> tt
                    -> label
                    -> Int -- finished
                    -> Int -- running
                    -> Int -- to be run
                    -> IO ()
printNoProgress change _ label finished_c running_c to_be_run_c = return ()

printProgress :: (Show label)
                 => (Ord tt)
                 => TaskStateChanged 
                    -> tt
                    -> label
                    -> Int -- finished
                    -> Int -- running
                    -> Int -- to be run
                    -> IO ()
printProgress change _ label finished_c running_c to_be_run_c =
  let action = case change of
                  TSCStarted -> "started: "
                  TSCStopped -> "finished: "
                  TSCTaskAdded -> "scheduled: "
      total = finished_c + running_c + to_be_run_c
      fi :: (Int -> Double) = fromIntegral
  in printf "==> %s %s  %d of %d  %.2f %% running: %d\n" action (show label) to_be_run_c total (((/) `on` fi) (100 * (total - to_be_run_c)) total) running_c


-- tt = task type
-- label = string describing running task
-- r = result of task
data Task tt label r = Task tt label ((Task tt label r -> IO ()) -> IO r)

runNewTask :: Task tt label r -> IO ()
runNewTask (Task _ _ task) = task runNewTask >> return ()


-- run tasks concurrently. By using task types you can specify that eg 2
-- downloads should run concurrently only but 10 calculations
-- Example:
-- runConcurrently (M.fromList [(1, 2),(2,10)]) [(1, "download 1",  \addT -> fetch "http://.." )] forkOS printProgress
runConcurrently :: 
                  forall label . forall tt . forall a .
                  (Show label)
                => (Ord tt)
                => M.Map tt Int
                -> (IO () -> IO ThreadId) -- which fork function to be used (one of forkIO forkOS)
                -> (TaskStateChanged 
                    -> tt
                    -> label
                    -> Int -- finished
                    -> Int -- running
                    -> Int -- to be run
                    -> IO ())
                -> [(Task tt label a)] -- the tasks: label, task type id, result
                -> IO (MVar [Either SomeException a])
runConcurrently max' fork progress tasks = do

  let toBeRunMap = M.fromListWith (++) [ (tt, [t])  | t@(Task tt _ _) <- tasks ]
  toBeRun <- newMVar $ toBeRunMap
  running <- newMVar $ M.empty
  acc_results <- newMVar []
  results <- newEmptyMVar

  let  -- sumV :: (Ord k) => M.Map k Int -> Int
       sumV = sum . M.elems
     
       prog change tt label = do
               (f,r,to) <- withMVar toBeRun $ \to' -> do
                       f <- withMVar acc_results $ return . length
                       (r :: Int) <- withMVar running $ return . sumV
                       return (f, r, (sum . map length . M.elems) to')
               progress change tt label f r to

       -- start a worker thread
       run (Task tt label action) = fork $ do
                                   res <- handle (\(e::SomeException) -> return (Left e)) (liftM Right (action addTask))
                                   modifyMVar_ running $ \m -> return (M.insert tt ((M.findWithDefault undefined tt m) - 1) m)
                                   prog TSCStopped tt label
                                   modifyMVar_ acc_results $ return . (res:)
                                   stateChanged

       stateChanged = do
         -- try starting tasks
         toBeRunT <- takeMVar toBeRun
         runningT <- takeMVar running
         finished_c <- withMVar acc_results $ return . length
         
         -- iterate over tasks to be run collecting tasks which can be run
         let (toBeRun2, running2, start) 
                   = M.foldrWithKey (\k v (map', running', s) ->
                         let r = M.findWithDefault 0 k runningT
                             m = M.findWithDefault 0 k max' -- if lookup fails it won't terminate! TODO add assertion
                             start' = take (m - r) v
                             v' = let rest = drop (m - r) v
                                  in if null rest then map' else M.insert k rest map'
                         in ( v'
                            , M.insertWith (+) k (length start') running'
                            , start' ++ s )
                     ) (M.empty, runningT, []) toBeRunT
         
         let running_c = sumV running2
         let to_be_run_c = sum $ map length $ M.elems toBeRun2
         -- starting new tasks:
         mapM_ (\t@(Task tt label _) -> do
                   _ <- run t
                   progress TSCStarted tt label finished_c running_c to_be_run_c) start
         putMVar running running2
         putMVar toBeRun toBeRun2
       
         -- finish condition:
         when (running_c == 0) $
           putMVar results =<< takeMVar acc_results

       addTask :: Task tt label a -> IO ()
       addTask task@(Task tt label _) = do
         modifyMVar_ toBeRun $ return . M.insertWith (++) tt [task]
         prog TSCTaskAdded tt label
         stateChanged

   in 
      if S.null (S.difference (M.keysSet toBeRunMap) (M.keysSet max'))
        then stateChanged >> return results
        else error "runConcurrently assertion failed: max is missing keys"
