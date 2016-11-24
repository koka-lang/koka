{-#OPTIONS -XNPlusKPatterns #-}
import Prelude hiding (min,(%),(/),mod,div)
import qualified Prelude


type Year  = Integer
type Week  = Integer
type Weekday  = Integer
type Weekdate = (Year,Week,Weekday)


monthdate( year, week+1, weekday+1 ) = (year, month+1, day+1)
  where
    day      = doy - daysBeforeMonth(month)
    month    = monthOf(doy)
    doy      = daysBeforeWeek(week) + weekday 

weekdate( year, month+1, day+1 ) = (year, week+1, weekday+1 )
  where
    weekday  = doy - daysBeforeWeek(week)
    week     = weekOf(doy)
    doy      = daysBeforeMonth(month) + day


monthOf(doy)            = min((doy - (doy+1)/91)/30, 11)
daysBeforeMonth(month)  = month*30 + month/3

weekOf(doy)             = doy/7
daysBeforeWeek(week)    = 7*week

weekdayOf(doy)          = doy - daysBeforeWeek(weekOf(doy))

--------------------------------------------------------------
-- 
--------------------------------------------------------------

isLong(year) = p(year) == 4 || p(year-1) == 3
  where
    p(y) = (y + y/4 - y/100 + y/400) % 7

weeksIn(year)  = if (isLong year) then 53 else 52

daysIn(year,month+1) = if (month==11 && isLong(year)) then 38
                         else if ((month+1)%3 == 0) then 31 
                         else 30



-- Testing

verify1a years
  = forWeekdates years $ \wd ->
    check "weekdate: " (showWd wd) (showWd $ weekdate (monthdate wd))

verify1b years
  = forMonthdates years $ \md ->
    check "monthdate: " (showMd md) (showMd $ monthdate (weekdate md))



-- Helpers


check msg expect chk
  | expect == chk = do putStrLn (msg ++ ": " ++ expect ++ ": ok")
  | otherwise     = do putStrLn (msg ++ ":FAILED!!!\n expect: " ++ expect ++ "\n gotten: " ++ chk)
                       error("test failed")


base0 (year,mw,dw) = (year,mw-1,dw-1)
base1 (year,mw,dw) = (year,mw+1,dw+1)

showMd(year,month,day)
  = show year ++ "-" ++ show2 month ++ "-" ++ show2 day

showWd(year,week,weekday)
  = show year ++ "-W" ++ show2 week ++ "-" ++ show weekday

show2 i
  = pad0 2 (show i)

pad0 n s
  = replicate (n - length s) '0' ++ s


forWeekdates :: Year -> (Weekdate -> IO ()) ->  IO ()
forWeekdates years action
  = mapM_ perYear [max(-5)(-years)..years] 
  where
    perYear year      = mapM_ (perWeek year) [1..weeksIn year]
    perWeek year week = mapM_ (\wday -> action(year,week,wday)) [1..7]

forMonthdates :: Year -> (Weekdate -> IO ()) ->  IO ()
forMonthdates years action
  = mapM_ perYear [max(-5)(-years)..years] 
  where
    perYear year        = mapM_ (perMonth year) [1..12]
    perMonth year month = mapM_ (\day -> action(year,month,day)) [1..daysIn(year,month)]


-- Floored division
(/),(%) :: Integer -> Integer -> Integer
(/) = fdiv
(%) = fmod

fdiv :: Integer -> Integer -> Integer
fdiv i j  | i < 0      = ((i+1) `Prelude.div` j) - 1
          | otherwise  = i `Prelude.div` j


fmod :: Integer -> Integer -> Integer
fmod i j  | i < 0      = i - (j*(i `fdiv` j))
          | otherwise  = i `Prelude.mod` j

min :: (Integer,Integer) -> Integer
min(i,j)  = if (i<=j) then i else j
