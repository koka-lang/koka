import Prelude hiding (min,(%),(/))
import qualified Prelude


type Year  = Integer
type Week  = Integer
type Weekday  = Integer
type Weekdate = (Year,Week,Weekday)


monthdate( year, week, wday ) = (year, month, day)
  where
    day    = doy - beforeMonth(month) 
    month  = monthOf(doy)
    doy    = 7*(week-1) + wday 

weekdate( year, month, day ) = (year, week, wday )
  where
    wday   = doy - 7*(week-1)
    week   = (doy-1)/7 + 1
    doy    = beforeMonth(month) + day 

monthOf(doy)        = min((100*doy)/3034 + 1, 12)
beforeMonth(month)  = 30*(month-1) + (month-1)/3


--------------------------------------------------------------
-- 
--------------------------------------------------------------

beforeMonthdate(year,month,day) 
  = beforeYear(year) + beforeMonth(month) + (day - 1) 

beforeYear(year) = gdays + adjust
  where
     adjust  = if (wday <= 4) then (1 - wday) else (8 - wday)
     wday    = (gdays%7) + 1
     gdays   = beforeGyear(year)

beforeGyear(gyear)
  = 365*y + y/4 - y/100 + y/400
  where
     y = gyear-1


--------------------------------------------------------------
-- 
--------------------------------------------------------------

monthdateOf(days) = (year,month,day)
   where
     day     = doy - beforeMonth(month)
     month   = monthOf(doy)
     doy     = days - beforeYear(year) + 1
     year    = if(days >= beforeYear(approx+1)) then approx+1 else approx
     approx  = gyearOf(days-3) 

gyearOf(days) 
  = 1 + (days - leapdays)/365
  where
    leapdays  = doe/1461 - doe/36525 + doe/146097
    doe       = days + 308


-----------------------------------------------------------
---
-- 
--------------------------------------------------------------


isLong(year) = gfirstdayOf(year)==4 || gfirstdayOf(year+1)==5

gfirstdayOf(gyear)
  = weekdayOf( gyear + leapdaysBefore )  
  where
    weekdayOf(doe) = ((doe-1)%7)+1
    leapdaysBefore = (gyear-1)/4 - (gyear-1)/100 + (gyear-1)/400


isLongX(year) = p(year) == 4 || p(year-1) == 3
  where
    p(y) = (y + y/4 - y/100 + y/400) % 7

lastweek(year)  = if (isLong year) then 53 else 52

lastday(year,month) = if (month==12 && isLong(year)) then 38
                      else if (month%3 == 0) then 31 
                      else 30



-- Testing

verify years
  = do verify1a years
       verify1b years
       verify2a years
       verify2b years
       verify3  years

verify1a years
  = forWeekdates years $ \wd ->
    check "weekdate: " (showWd wd) (showWd $ weekdate (monthdate wd))

verify1b years
  = forMonthdates years $ \md ->
    check "monthdate: " (showMd md) (showMd $ monthdate (weekdate md))

verify2a years
  = forMonthdates years $ \md ->
    check "monthdays: " (showMd md) (showMd $ monthdateOf (beforeMonthdate md))

verify2b years
  = forDays years $ \d ->
    let md = monthdateOf d in
    check ("daysmonth: " ++ showMd md) (show d) (show $ beforeMonthdate md)

verify3 years
  = mapM_ perYear [(-years)..years]
  where
    perYear year = check ("year: " ++ show year) (show (isLongX year)) (show (isLong year))
-- Helpers


check msg expect chk
  | expect == chk = do putStrLn (msg ++ ": " ++ expect ++ ": ok")
  | otherwise     = do putStrLn (msg ++ ":FAILED!!!\n expect: " ++ expect ++ "\n gotten: " ++ chk)
                       error("test failed")



showMd(year,month,day)
  = show4 year ++ "-" ++ show2 month ++ "-" ++ show2 day

showWd(year,week,weekday)
  = show4 year ++ "W" ++ show2 week ++ "-" ++ show weekday

show4 i
  = pad0 4 (show i)

show2 i
  = pad0 2 (show i)

pad0 n s
  = replicate (n - length s) '0' ++ s


forWeekdates :: Year -> (Weekdate -> IO ()) ->  IO ()
forWeekdates years action
  = mapM_ perYear [max(-5)(-years)..years] 
  where
    perYear year      = mapM_ (perWeek year) [1..lastweek year]
    perWeek year week = mapM_ (\wday -> action(year,week,wday)) [1..7]

forMonthdates :: Year -> (Weekdate -> IO ()) ->  IO ()
forMonthdates years action
  = mapM_ perYear [max(-5)(-years)..years] 
  where
    perYear year        = mapM_ (perMonth year) [1..12]
    perMonth year month = mapM_ (\day -> action(year,month,day)) [1..lastday(year,month)]

forDays :: Year -> (Integer -> IO ()) ->  IO ()
forDays years action
  = mapM_ (\day -> action day) [366*max(-5)(-years)..years*366] 
  

-- Euclidean division
(/),(%) :: Integer -> Integer -> Integer
(/) = ediv
(%) = emod

ediv :: Integer -> Integer -> Integer
ediv i d  = let (q,r) = i `divMod` d
            in if (r>=0) then q else if (d > 0) then q-1 else q+1

emod :: Integer -> Integer -> Integer
emod i d  = let r = i `mod` d
            in if (r >= 0) then r else if (d > 0) then r+d else r-d

min :: (Integer,Integer) -> Integer
min(i,j)  = if (i<=j) then i else j
