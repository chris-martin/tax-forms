module TaxForms.Utility where

import Control.Monad.Reader
import Optics
import Relude hiding (subtract)

askFor :: (MonadReader a m,  Is k A_Getter) => Optic' k is a b -> m b
askFor o = ask <&> view o

add :: (Applicative f, Num a) => f a -> f a -> f a
add = liftA2 (+)

multiply :: (Applicative f, Num a) => f a -> f a -> f a
multiply = liftA2 (*)

percent :: (Applicative f, Num a, Fractional a) => a -> f a -> f a
percent p = multiply (pure (p / 100))

-- | @subtract deduction original@ means "subtract deduction from original"
--
-- Notice that this isn't the subtraction operator (-); it is subtraction flipped. We write it this way because this is how the form instructions are written.
subtract :: (Applicative f, Num a) => f a -> f a -> f a
subtract = flip (liftA2 (-))

smaller :: (Applicative f, Ord a) => f a -> f a -> f a
smaller = liftA2 min

if_zero_or_less_enter_0 :: (Functor f, Num a, Ord a) => f a -> f a
if_zero_or_less_enter_0 = fmap (\x -> if x <= 0 then 0 else x)

showDollarsAndCents :: RealFrac a => a -> String
showDollarsAndCents x = show dollars <> "." <> padCents (show (abs cents))
  where
    (dollars, cents) = quotRem (round (x * 100) :: Integer) 100
    padCents s = replicate (2 - length s) '0' <> s
