# Imperfect 100 percent

## Issue with round

In real world, deal with float-point number isn't easy as we think. Fixed-point number can be very very long that human's eyes can't read. Some programming languages such as JavaScript that don't support big numbers, the only solution is converting to string. This is the common case if you work with Databases.

To make float-point number read and compute, the most simple choice is rounding it. Most of problems was solved until we need to calculate percent statistics. Normally, it is solved easily with simple steps: sum, divide, and round:

```haskell
calculatePercent :: List Double -> List Double 
calculatePercent xs = map (\x -> round' $ x / total) xs
  where
    total = sum xs
    round' n = (round (x * 10000.0)) / 100.0 -- // round to 2 fixed point numbers
```

It seems okay. Round number also causes accuracy lost. The sum of percent values aren't always exact `100`, the result range can be from `99` to `101`, called [Round-off Error](https://en.wikipedia.org/wiki/Round-off_error)

```
[3, 3, 3]

33.33 + 33.33 + 33.33 = 99.99
```

## Work Around

According to [Stack Overflow Question][1], there are several way to work around this issue. In brief, we'll round each candidate until the sum is 100. Candidates are determined by largest value, or minimum rounding error 

```haskell
import Data.List (sort, sortBy, drop, take)
import GHC.Exts (sortWith)

betterPercent :: [Double] -> [Double] 
betterPercent xs = map ((\x -> x / 100.0) . snd) $ sortWith fst newTValues
  where
    total = sum xs
    scaleFactor = 10000.0
    
    floor' :: Double -> Double
    floor' n = fromIntegral $ floor (n * scaleFactor)
    
    flooredValues :: [Double]
    flooredValues = map (\x -> floor' $ x / total) xs
    
    needToRound :: Int
    needToRound = fromIntegral $ floor $ scaleFactor - (sum flooredValues)
    
    tupledValues = reverse $ sortWith snd $ zip [1..] flooredValues
    
    newTValues :: [(Int, Double)]
    newTValues = (map (\(i, x) -> (i, x + 1.0)) $ take needToRound tupledValues) <> drop needToRound tupledValues

```

```
[3, 3, 3]
33.33 + 33.33 + 33.34 = 100
```

Hooray, the sum is `100` now! However, member values is still inaccurate. Everyone will ask:

> Why 33.34 = 33.33?

We can't be sure how to explain this. Explain by mathematic knowledge, or just: because it is real number

> I also do a simple library written in PureScript [Round Ratio](https://github.com/hgiasac/purescript-round-ratio)

## Rational Number

The most accuracy solution is using Fraction number. In mathematic, the form of fraction number is 1/2. In Haskell, there is `Rational` type:

```haskell
n :: Rational
n = 1 % 2
```

We just keep the original form with numerator and denominator and use them to compute when necessary. However, the performance is slower than float-point number. When print the value, we also need to convert to fixed-point value for easier understanding. We can't ask the user recalculate fraction values over and over again.

Sometimes we have to choose between accuracy and performance...

## References

1. [How to make rounded percentages add up to 100%][1]


[1]: https://stackoverflow.com/questions/13483430/how-to-make-rounded-percentages-add-up-to-100
