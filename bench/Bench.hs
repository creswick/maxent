{-# LANGUAGE TupleSections, Rank2Types #-}
module Main where
import Numeric.MaxEnt.Internal
import Criterion.Main
import Criterion
import Criterion.Config
import Data.Monoid
import qualified Data.Vector.Storable as S
   


myConfig = defaultConfig { cfgReport = Last $ Just "profile.html" ,
                           cfgSamples = Last $ Just 100}

main = defaultMainWith myConfig (return ()) [
           bgroup "linear" [
               bench "linear1"  $ nf ((\(Right x) -> x) . linear 3.0e-17) (LC [[0.68, 0.22, 0.1], [0.1, 0.68, 0.22], [0.22, 0.1, 0.68]] [0.276, 0.426, 0.298]),
               bench "linear'"  $ nf ((\(Right x) -> x) . linear 3.0e-17) (LC [[0.68, 0.22, 0.1], [0.1, 0.68, 0.22], [0.22, 0.1, 0.68]] [0.276, 0.426, 0.298]),
               bench "linear''" $ nf ((\(Right x) -> x) . linear 3.0e-17) (LC [[0.68, 0.22, 0.1], [0.1, 0.68, 0.22], [0.22, 0.1, 0.68]] [0.276, 0.426, 0.298])
           ]
       ]