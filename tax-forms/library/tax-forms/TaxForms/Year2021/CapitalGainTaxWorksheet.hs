-- | Form 1040, Line 16: Qualified Dividends and Capital Gain Tax Worksheet

module TaxForms.Year2021.CapitalGainTaxWorksheet where

import Control.Monad.Reader
import Optics
import Relude hiding (subtract)
import TaxForms.Utility

data Input = Input
  { inputTaxableIncome :: Rational
      -- ^ Enter the amount from Form 1040 or 1040-SR, line 15. However, if you are filing Form 2555 (related to foreign earned income), enter the amount from line 3 of the Foreign Earned Income Tax Worksheet
  , inputQualifiedDividends :: Rational
      -- ^ Enter the amount from Form 1040 or 1040-SR, line 3a (see footnote)
  , inputCapitalGainOrLoss :: Rational
      -- ^ Are you filing Schedule D? (see footnote)
      --
      -- If yes: Enter the smaller of line 15 or 16 of Schedule D. If either line 15 or 16 is blank or a loss, enter 0.
      --
      -- If no: Enter the amount from Form 1040 or 1040-SR, line 7.
  , inputFilingStatus :: FilingStatus
  }
-- ^ Footnote: If you are filing Form 2555, see the footnote in the Foreign Earned Income Tax Worksheet before completing this line.

data FilingStatus =
    Single
  | MarriedFilingSeparately
  | MarriedFilingJointly -- ^ or qualifying widow(er)
  | HeadOfHousehold

makeLensesWith camelCaseFields ''Input

newtype Form a = Form (Input -> a)
  deriving stock Functor
  deriving (Applicative, Monad, MonadReader Input)
    via (Reader Input)

runForm :: Form a -> Input -> a
runForm (Form f) i = f i

line1 :: Form Rational
line1 = askFor taxableIncome

line2 :: Form Rational
line2 = askFor qualifiedDividends

line3 :: Form Rational
line3 = askFor capitalGainOrLoss

line4 :: Form Rational
line4 = add line2 line3

line5 :: Form Rational
line5 = if_zero_or_less_enter_0 (subtract line4 line1)

line6 :: Form Rational
line6 = askFor filingStatus <&> \case
    Single                  -> 40_400
    MarriedFilingSeparately -> 40_400
    MarriedFilingJointly    -> 80_800
    HeadOfHousehold         -> 54_100

line7 :: Form Rational
line7 = smaller line1 line6

line8 :: Form Rational
line8 = smaller line5 line7

line9 :: Form Rational
line9 = subtract line8 line7

line10 :: Form Rational
line10 = smaller line1 line4

line11 :: Form Rational
line11 = line9

line12 :: Form Rational
line12 = subtract line11 line10

line13 :: Form Rational
line13 = askFor filingStatus <&> \case
    Single                  -> 445_850
    MarriedFilingSeparately -> 250_800
    MarriedFilingJointly    -> 501_600
    HeadOfHousehold         -> 473_750

line14 :: Form Rational
line14 = smaller line1 line13

line15 :: Form Rational
line15 = add line5 line9

line16 :: Form Rational
line16 = if_zero_or_less_enter_0 (subtract line15 line14)

line17 :: Form Rational
line17 = smaller line12 line16

line18 :: Form Rational
line18 = percent 15 line17

line19 :: Form Rational
line19 = add line9 line17

line20 :: Form Rational
line20 = subtract line19 line10

line21 :: Form Rational
line21 = percent 20 line20
