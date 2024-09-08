module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- INTRODUCTION

type MoneyF = Float

rateOfReturn :: MoneyF -> MoneyF -> MoneyF
rateOfReturn s0 s1 = (S1 - S0) / S0 -- Ks is rate of return or return

-- n = period of time
-- r = interest rate

type PeriodOfTime = Int
type InterestRate = MoneyF

simpleInterest :: MoneyF -> InterestRate -> PeriodOfTime -> MoneyF
simpleInterest cv r n = cv * r * (fromIntegral n :: Float)

rateOfInterest :: MoneyF -> MoneyF -> PeriodOfTime -> InterestRate
rateOfInterest cv i n = i / (cv * periodFloat)
    where  periodFloat = fromIntegral n :: Float

rateOfInterest :: MoneyF -> InterestRate -> PeriodOfTime -> Float
futureValue cv r n = cv * (1 + (r * periodFloat))
    where  periodFloat = fromIntegral n :: Float

periodFvCv :: MoneyF -> MoneyF -> InterestRate -> Float
periodFvCv fv cv r = ((fv / cv) - 1) / r