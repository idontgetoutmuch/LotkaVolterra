{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Data.Random
import           Data.Random.Source.PureMT ( pureMT )
import           Control.Monad.State ( evalState, replicateM )
import qualified Control.Monad.Loops as ML
import           Control.Monad.Writer ( tell, WriterT, lift,
                                        runWriterT
                                      )

import           Data.List (zipWith4)

import           Frames
import           Frames.CSV
import           Data.Foldable (toList)
import           Control.Lens((^.))

import           System.IO.Unsafe (unsafePerformIO)

import qualified Numeric.LinearAlgebra as LA
import           Numeric.GSL.ODE

tableTypes "NoisyObs" "lynx_hare_df.csv"

loadNoisyObs :: IO (Frame NoisyObs)
loadNoisyObs = inCoreAoS (readTable "lynx_hare_df.csv")

predPreyObs :: [((Int, Double), (Int, Double))]
predPreyObs = unsafePerformIO $
          do xs <- loadNoisyObs
             let us = toList $ fmap (\u -> ((u ^. year), (u ^. hare))) xs
             let vs = toList $ fmap (\u -> ((u ^. year), (u ^. lynx))) xs
             return $ zip us vs

ts :: [Double]
ts = map (fromIntegral . fst . fst) predPreyObs

nMeasTs, nColonies :: Int
nMeasTs = length ts
nColonies = 2

initPop :: (Double, Double)
initPop = (snd $ fst $ head predPreyObs, snd $ snd $ head $ predPreyObs)

data Rate = Rate { theta1 :: Double, theta2 :: Double, theta3 :: Double, theta4 :: Double }
  deriving Show

meanRate :: Rate
meanRate = Rate { theta1 = 0.5, theta2 = 0.025, theta3 = 0.8, theta4 = 0.025 }

sampledRate :: Rate -> IO Rate
sampledRate r = do
  t1 <- exp <$> (sample $ rvar $ Normal (log (theta1 r)) 0.1)
  t2 <- exp <$> (sample $ rvar $ Normal (log (theta2 r)) 0.015)
  t3 <- exp <$> (sample $ rvar $ Normal (log (theta3 r)) 0.1)
  t4 <- exp <$> (sample $ rvar $ Normal (log (theta4 r)) 0.015)
  return $ Rate { theta1 = t1, theta2 = t2, theta3 = t3, theta4 = t4 }

dzdt :: Rate -> Double -> [Double] -> [Double]
dzdt r _t [u, v] = [ (alpha - beta * v) * u
                   , (-gamma + delta * u) * v
                   ]
  where
    alpha = theta1 r
    beta = theta2 r
    delta = theta4 r
    gamma = theta3 r

singleDataSet :: Int -> IO ()
singleDataSet n = do
  r <- sampledRate meanRate
  print r
  let m = odeSolve (dzdt r) [fst initPop, snd initPop] (LA.vector [1.0, 2.0..(fromIntegral nMeasTs)])
  let cs = LA.toColumns m
      hs = cs!!0
      ls = cs!!1
      genDf :: Frame NoisyObs
      genDf = boxedFrame $
              zipWith4 (\x y u v -> v &: u &: y &: x &: Nil)
                       (LA.toList hs) (LA.toList ls) [1900..] [1..]
  writeCSV ("generatedLV" ++ (show n) ++ ".csv") genDf


main :: IO ()
main = do
  mapM_ singleDataSet [1..nColonies]
