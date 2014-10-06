{-# LANGUAGE TupleSections, Rank2Types #-}
module Numeric.MaxEnt.General (
    Constraint,
    general
 ) where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Internal.Tower
import Numeric.AD.Internal.Classes
import Data.List (transpose)
import Control.Applicative
import Numeric.AD.Lagrangian

entropy :: Floating a => [a] -> a
entropy xs = negate . sum . map (\x -> x * log x) $ xs

-- | A more general solver. This directly solves the lagrangian of the constraints and the
--  the additional constraint that the probabilities must sum to one.
general :: Double 
        -- ^ Tolerance for the numerical solver
        -> Int
        -- ^ the count of probabilities
        -> [Constraint Double]
        -- ^  constraints
        -> Either (Result, Statistics) (S.Vector Double) 
        -- ^ Either the a discription of what wrong or the probability distribution
general tolerance count constraints = 
    fst <$> maximize tolerance entropy ((sum <=> 1.0) : constraints) count
 
   