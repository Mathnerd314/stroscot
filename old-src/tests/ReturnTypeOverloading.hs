{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative (Applicative, (<*>))
import Text.Read (readMaybe)
import Prelude (IO (..), Maybe (..), Monad, Num (..), getLine, putStrLn, return, show, ($), (++), (<$>), (<*>))
import qualified Prelude as P

default (P.Double)

main1 = do
  putStrLn "Please enter your birth year"
  yearString <- getLine
  case readMaybe yearString of
    Nothing -> putStrLn "You provided an invalid year"
    Just y -> putStrLn $ "In 2025, you will be: " ++ show (2025 - y)

displayAge maybeAge =
  case maybeAge of
    Nothing -> putStrLn "You provided an invalid year"
    Just y -> putStrLn $ "In 2025, you will be: " ++ show y

yearToAge year = 2025 - year

main2 = do
  putStrLn "Please Enter your birth year"
  yearString <- getLine
  let maybeAge = case readMaybe yearString of
        Nothing -> Nothing
        Just year -> Just (yearToAge year)
  displayAge maybeAge

-- Functors

main3 = do
  putStrLn "Please enter your birth year"
  yearString <- getLine
  let maybeAge = fmap yearToAge (readMaybe yearString)
  displayAge maybeAge

main4 = do
  putStrLn "Please enter your birth year"
  birthYearString <- getLine
  putStrLn "Please enter some year in the future"
  futureYearString <- getLine
  let maybeAge =
        case readMaybe birthYearString of
          Nothing -> Nothing
          Just birthYear ->
            case readMaybe futureYearString of
              Nothing -> Nothing
              Just futureYear -> Just (futureYear - birthYear)
  displayAge maybeAge

yearDiff futureYear birthYear = futureYear - birthYear

main5 = do
  putStrLn "Please enter your birth year"
  byStr <- getLine
  putStrLn "Please enter your future year"
  fyStr <- getLine
  let maybeAge = do
        birthYear <- readMaybe byStr
        futureYear <- readMaybe fyStr
        return $ yearDiff futureYear birthYear
  displayAge maybeAge

main6 = do
  putStrLn "Please enter your birth year"
  byStr <- getLine
  putStrLn "Please enter some year in the future"
  fyStr <- getLine
  let maybeAge = do
        yearToAge <- fmap yearDiff (readMaybe fyStr) -- lift yearDiff over Maybe Integer to get Maybe (Integer -> Integer)
        birthYear <- readMaybe byStr
        return $ yearToAge birthYear
  displayAge maybeAge

-- using applicative to re-write the above

main7 = do
  putStrLn "Please enter your birth year"
  byStr <- getLine
  putStrLn "Please enter some year in the future"
  fyStr <- getLine
  let maybeAge =
        fmap yearDiff (readMaybe fyStr) <*> readMaybe byStr -- Maybe (Integer -> Integer) <*> Maybe Integer = Maybe Integer
  displayAge maybeAge

-- another way of writing the above using <$> and <*>

main8 = do
  putStrLn "Please enter your birth year"
  byStr <- getLine
  putStrLn "Please enter some year in the future"
  fyStr <- getLine
  let maybeAge =
        yearDiff <$> readMaybe fyStr <*> readMaybe byStr -- Maybe (Integer -> Integer) <*> Maybe Integer = Maybe Integer
  displayAge maybeAge

main9 = do
  putStrLn "Please enter your birth year"
  byStr <- getLine
  putStrLn "Please enter some year in the future"
  fyStr <- getLine
  let maybeAge = do
        futureYear <- readMaybe fyStr
        birthYear <- readMaybe byStr
        return $
          if futureYear P.< birthYear
            then yearDiff birthYear futureYear
            else yearDiff futureYear birthYear
  displayAge maybeAge

--- Exercise 1

fmap f ma = return f <*> ma

main10 =
  case fmap (P.+ 1) (P.Just 2) of
    P.Just 3 -> P.putStrLn "Good job!"
    _ -> P.putStrLn "Try again"

--- Exercise 2

returnMaybe2 = Just

main11
  | returnMaybe2 "Hello" P.== Just "Hello" = putStrLn "Correct!"
  | P.otherwise = putStrLn "Incorrect, please try again"

-- Exercise 3

main12 = do
  putStrLn "Please enter your birth year"
  byStr <- getLine
  putStrLn "Please enter some year in the future"
  fyStr <- getLine
  let maybeAge = do
        futureYear <- readMaybe fyStr
        birthYear <- readMaybe byStr
        return $
          if futureYear P.< birthYear
            then (-) birthYear futureYear
            else (-) futureYear birthYear
  displayAge maybeAge

-- Exercise 4

yearDiff2 futureYear birthYear
  | futureYear P.> birthYear = futureYear - birthYear
  | P.otherwise = birthYear - futureYear

-- Exercise 5

yourHelperFunction f n1 n2
  | n1 P.> n2 = f n1 n2
  | P.otherwise = f n2 n1

main13
  | yourHelperFunction yearDiff 5 6 P.== 1 = putStrLn "Correct!"
  | P.otherwise = putStrLn "Please try again"
