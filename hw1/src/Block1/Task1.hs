module Block1.Task1
       ( Day (..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       ) where

-- |Data structure for holding days of week
data Day = Mon
         | Tue
         | Wed
         | Thu
         | Fri
         | Sat
         | Sun
         deriving (Show, Eq) -- deriving Eq only for tests
                             -- and it is easy to do by hands
                             -- so forgive me, I am lazy :(

-- |Next day
nextDay :: Day -> Day
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

-- |Day after given amount of days
afterDays :: Day -> Int -> Day
afterDays day count | count > 0 = afterDays (nextDay day) (count - 1)
                    | otherwise = day

-- |Is weekend
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- |Returns amount of days until friday from given day
daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = 1 + daysToParty (nextDay day)
