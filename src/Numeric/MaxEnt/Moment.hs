{-# LANGUAGE TupleSections, Rank2Types, NoMonomorphismRestriction #-}
module Numeric.MaxEnt.Moment (
        ExpectationConstraint,
        (.=.),
        ExpectationFunction,
        average,
        variance,
        maxent,
        UU(..)
    ) where
import Numeric.Optimization.Algorithms.HagerZhang05
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Numeric.AD
import GHC.IO                   (unsafePerformIO)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Internal.Classes
import Data.List (transpose)
import Control.Applicative
import Numeric.MaxEnt.ConjugateGradient
import Data.List (foldl')
--import Data.Vector

-- | Constraint type. A function and the constant it equals.
-- 
--   Think of it as the pair @(f, c)@ in the constraint 
--
-- @
--     &#931; p&#8336; f(x&#8336;) = c
-- @
--
--  such that we are summing over all values .
--
--  For example, for a variance constraint the @f@ would be @(\\x -> x*x)@ and @c@ would be the variance.
type ExpectationConstraint a = (UU a, a)

--
infixr 1 .=.
(.=.) :: (forall s. Mode s => AD s a -> AD s a) -> a -> ExpectationConstraint a
f .=. c = (UU f, c)

-- | A function that takes an index and value and returns a value.
--   See 'average' and 'variance' for examples.
type ExpectationFunction a = (a -> a)

newtype UU a = UU {unUU :: forall s. Mode s => ExpectationFunction (AD s a) }

-- The average constraint
average :: Num a => a -> ExpectationConstraint a
average m = id .=. m

-- The variance constraint
variance :: Num a => a -> ExpectationConstraint a
variance sigma = (^(2 :: Int)) .=. sigma

--partialPart' ls fs x = exp . negate . S.sum . S.zipWith (\l f -> l * f x) ls $ fs
--partitionFunc' values fs ls = S.sum . S.map (partialPart' ls fs) $ values

probs values fs ls = result where
    lsList    = S.toList ls
    norm      = partitionFunc values fs lsList
    result    = S.map (\x -> partialPart lsList fs x / norm) $ S.fromList values 

partialPart ls fs x = exp . negate . sum . zipWith (\l f -> l * f x) ls $ fs

partitionFunc values fs ls = sum . map (partialPart ls fs) $ values

objectiveFunc fs moments values ls = 
    log (partitionFunc values fs ls) + (sum $ zipWith (*) ls moments)

-- | Discrete maximum entropy solver where the constraints are all moment constraints. 
maxent :: Double 
       -- ^ Tolerance for the numerical solver
       -> [Double]
       -- ^ values that the distributions is over
       -> [ExpectationConstraint Double]
       -- ^ The constraints
       -> Either (Result, Statistics) (S.Vector Double) 
       -- ^ Either the a discription of what wrong or the probability distribution 
maxent tolerance values constraints = result where
    obj = objectiveFunc (map unUU fs') (map auto moments) (map auto values)
    
    count = length fs
        
    (fs', moments) = unzip constraints 
    
    fs = map (\x -> lowerUU $ unUU x) fs'
    
    guess = U.fromList $ replicate count (1.0 / fromIntegral count :: Double) 
    
    result =  probs values fs <$> minimize tolerance count obj
