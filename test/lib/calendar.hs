import qualified Data.Time as T
import qualified Data.Time.Calendar.WeekDate as W

infix 7 //

--------------------------------------------------------------
-- 
--------------------------------------------------------------

negone x  = if (x < 0) then 1 else 0

(//) :: Integer -> Integer -> Integer
x // y    = (x `div` y) + negone y

(%%) :: Integer -> Integer -> Integer
x %% y    = (x `mod` y) - negone y

(/%) ::  Integer -> Integer -> (Integer,Integer)
x /% y    = (x//y, x%%y)

--------------------------------------------------------------
-- 
--------------------------------------------------------------

type Year  = Integer
type Month = Integer
type Day   = Integer
type Week  = Integer
type Doy   = Integer
type Doe   = Integer
type Days  = Integer

data Date  = Date{ year :: Year, month :: Month, day :: Day }
           deriving (Eq,Ord)


instance Show Date where
  show (Date year month day) = show0 4 year ++ "-" ++ show0 2 month ++ "-" ++ show0 2 day

show0 n i  
  = sign ++ replicate (n - length s) '0' ++ s
  where
    sign = if (i < 0) then "-" else ""
    s    = show (abs i)

--------------------------------------------------------------
-- 
--------------------------------------------------------------
gdoyFromMonth :: Year -> Month -> Doy  
gmonthFromDoy :: Year -> Doy -> Month  -- =
gdoeFromYear  :: Year -> Doe           
gestimateYear :: Doe -> Year           -- =


gdoyFromMonth year month 
  = (367*month - 362)//12 - adj
  where
    adj = gadjust (month<=2) year

gmonthFromDoy year doy 
  = (12*(doy + adj) + 373)//367
  where
    adj = gadjust (doy<=58) year

gadjust beforeMarch year  
  | beforeMarch  = 0
  | gisLeap year = 1
  | otherwise    = 2
    
gisLeap year     
  = (year%%4 == 0) && (year%%100/=0 || year%%400==0)

gdoeFromYear year
  = 1 + 365*year + y//4 - y//100 + y//400
  where
    y = year - 1

gestimateYear doe
  = era*400 + (100*doy)//36525
  where 
    (era,doy) = doe /% 146097

gregorian :: Calendar
gregorian = ecalendar (gdoeFromYear, gestimateYear) (gdoyFromMonth, gmonthFromDoy)


--------------------------------------------------------------
-- 
--------------------------------------------------------------

type ConvYear   = (Year -> Doe, Doe -> (Year,Doy))
type ConvMonth  = (Year -> Month -> Doy, Year -> Doy -> Month)
type ConvEstYear= (Year -> Doe, Doe -> Year)

data Calendar   = Calendar{ fromDate :: Date -> Doe, fromDoe :: Doe -> Date }

calendar :: ConvYear -> ConvMonth -> Calendar
calendar (doeFromYear,yeardoyFromDoe) (doyFromMonth,monthFromDoy)  -- =
  = Calendar doeFromDate dateFromDoe
  where
     doeFromDate (Date year month day)
       = doeFromYear year + doyFromMonth year month + day - 1

     dateFromDoe doe
       = Date year month day
       where
          day        = doy - doyFromMonth year month + 1
          month      = monthFromDoy year doy
          (year,doy) = yeardoyFromDoe doe 

ecalendar :: ConvEstYear -> ConvMonth -> Calendar
ecalendar (doeFromYear,estimateYear) convMonth           -- =
  = calendar (doeFromYear,yeardoyFromDoe) convMonth
  where
    yeardoyFromDoe doe = (year,doy)
      where
        doy    = doe - doeFromYear year
        year   = if doe >= doeFromYear (approx+1) then approx+1 else approx
        approx = estimateYear doe 





--------------------------------------------------------------
-- 
--------------------------------------------------------------

test years
  = forDays years $ \doe ->
    let date   = fromDoe gbuiltin doe
        gdate  = fromDoe gregorian doe
        gdoe   = fromDate gregorian gdate
    in if (date==gdate && gdoe==doe) then return () else
        putStrLn $ "FAIL: date: " ++ show (date,gdate) ++ ", days: " ++ show (doe,gdoe) 

gbuiltin :: Calendar
gbuiltin 
  = Calendar doeFromDate dateFromDoe
  where    
    dateFromDoe doe 
      = Date y (fromIntegral m) (fromIntegral d)
      where 
        (y,m,d) = T.toGregorian (T.addDays doe epoch)
      
    doeFromDate (Date year month day)
      = T.diffDays (T.fromGregorian year (fromIntegral month) (fromIntegral day)) epoch

    epoch :: T.Day
    epoch = T.fromGregorian 0 1 1  



forDays :: Year -> (Integer -> IO ()) ->  IO ()
forDays years action
  = mapM_ (\day -> action day) [366*max(-5)(-years)..years*366] 
  

--------------------------------------------------------------
-- 
--------------------------------------------------------------


--------------------------------------------------------------
-- 
--------------------------------------------------------------

{-
type Year  = Integer
type Month = Integer
type Day   = Integer
type Week  = Integer
type Weekday  = Integer
type Weekdate = (Year,Week,Weekday)

type Date = (Year,Month,Day)
type Days = Integer
type Doy  = Integer

rcalendar :: (Year -> Days, Days -> Year, Month -> Doy, Doy -> Month) -> 
            (Date -> Days, Days -> Date)
rcalendar (beforeYear, eyearOf, beforeMonth, monthOf)
  = ecalendar (beforeYear, eyearOf, beforeMonth . snd, monthOf . snd)


ecalendar :: (Year -> Days, Days -> Year, (Year,Month) -> Doy, (Year,Doy) -> Month) -> 
            (Date -> Days, Days -> Date)
ecalendar (beforeYear, estimateYear, beforeMonth, monthOf ) 
  = calendar(beforeYear, yearOf, beforeMonth, monthOf )
  where
    yearOf(days) = (year,doy)
      where
        doy    = days - beforeYear(year) + 1
        year   = if (days >= beforeYear(approx+1)) then approx+1 else approx
        approx = estimateYear(days) 


calendar :: (Year -> Days, Days -> (Year,Doy), (Year,Month) -> Doy, (Year,Doy) -> Month) -> 
            (Date -> Days, Days -> Date)

calendar (beforeYear, yearOf, beforeMonth, monthOf)
  = (beforeDate, dateOf)
  where
     beforeDate(year,month,day)
       = beforeYear(year) + beforeMonth(year,month) + (day - 1)

     dateOf(days)
       = (year,month,day)
       where
          day   = doy - beforeMonth(year,month) 
          month = monthOf(year,doy)
          (year,doy) = yearOf days 



(gbeforeDate,gdateOf) 
  = ecalendar (beforeGyear, estimateYear, gbeforeMonth, gmonthOf)
  
(wbeforeDate,wdateOf)
  = rcalendar (beforeYear, estimate, beforeWeek, weekOf )
  where
    estimate(days) = estimateYear(days-3)

(mbeforeDate,mdateOf)
  = rcalendar (beforeYear, estimate, beforeMonth, monthOf )
  where
    estimate(days) = estimateYear(days-3)


beforeGdate( year, month, day )
  = beforeGyear(year) + gbeforeMonth(month,year) + (day - 1)
  
xgdateOf( days )
  = ((year,month,day))
  where
    day        = doy - gbeforeMonth(year,year) 
    month      = gmonthOf(year,doy)
    (year,doy) = gdoydateOf days


gbeforeMonth( year, month )  = (367*(month) - 362)/12 - adj
  where
    adj = gadjust(month<=2,year)

gmonthOf( year, doy ) = ((12*(doy - 1 + adj) + 373) / 367)
  where
    adj = gadjust(doy<=59,year)

gadjust( beforeMar, year )  | beforeMar    = 0
                            | gisLeap year = 1
                            | otherwise    = 2
    
gisLeap( year )    = (year%4 == 0) && (year%100/=0 || year%400==0)


gdoydateOf( days ) = (year,doy)
  where
    doy    = days - beforeGyear(year) + 1
    year   = if (days >= beforeGyear(approx+1)) then approx+1 else approx    
    approx = estimateYear(days)



testGdate years
  = forDays years $ \days ->
    let d1 = gregdateOf days
        (d2) = gdateOf days
        days2 = gbeforeDate d2
        w1 = gregWeekdateOf days
        w2 = wdateOf days
    in if (d1==d2 && days==days2 && w1 == w2) then return () else
       putStrLn("diff: " ++ show (d1,d2) ++ ", week: " ++ show (w1,w2) ++ ", days=" ++ show (days,days2) )-- ++ ", corr/doy:" ++ show (corr,doy))


testApprox years
  = forDays years $ \days ->
    let approx = estimateYear(days) -- floor( (Prelude./) (fromIntegral day) 365.25 ) + 1
        gapprox = gregYearOf days -- gyearOf( days ) in
    in if (approx==gapprox)  
     then return ()
     else let doy = days - beforeGyear(approx)
              approx2 = if (doy >= 365) then approx+1 else approx -- estimate is always correct just after a leap year!
          in putStrLn("diff: " ++ show days ++ ", " ++ show approx ++ " /= " ++ show gapprox ++ ", " ++ (if (approx2==gapprox) then "OK" else if (approx+1 == gapprox) then "ok" else "FAIL") ++ ", " ++ show approx2)


gregYearOf(days) = let (y,m,d) = gregdateOf days in y

gregdateOf :: Integer -> Date
gregdateOf(days) = let (y,m,d) = T.toGregorian (T.addDays days epoch)
                   in (y,fromIntegral m,fromIntegral d)

gregWeekdateOf :: Integer -> Weekdate
gregWeekdateOf(days) = let (y,w,wd) = W.toWeekDate (T.addDays days epoch)
                      in (y,fromIntegral w,fromIntegral wd)

epoch :: T.Day
epoch = T.fromGregorian 1 1 1  

--------------------------------------------------------------
-- 
--------------------------------------------------------------
weeksToYear0 :: Integer -> Integer

weeksToYear weeks
  = 1 + (w - w/208 - w/416 + w/417)/52
    -- 1 + (w - w/208 + w/(4*208) + w/(32*208) + w/(48*208) ) /52 -- 14
    -- 1 + (w - w/208 + w/(4*208) + w/(23*208))/52
  where
    w= weeks

weeksToYear0 weeks 
  = round year
  where
    year :: Double
    year = (1.904557104e-2*(fromIntegral weeks) + 5.063163942e-1)

checkWeekYear n
  = filter pred (map addwy (weekyears n))
  where
    addwy (weeks,year) = (weeks,year,weeksToYear weeks)
    pred (weeks,year,wy) = wy /= year

weekyears n
  =  (map pairw [1..n*53])
  where
    pair i =
      map pairw ([(i*52 - 1)..(i*52 + 1)] ++ [(i*53 - 1)..(i*53 + 1)])
    pairw week = 
      let (year,_,_) = weekdateOf(7*(week-1))
      in (week-1,year)



--------------------------------------------------------------
-- 
--------------------------------------------------------------


monthdate( year, week, wday ) = (year, month, day)
  where
    day    = doy - beforeMonth(month) 
    month  = monthOf(doy)
    doy    = beforeWeek(week) + wday 


weekdate( year, month, day ) = (year, week, wday )
  where
    wday   = doy - beforeWeek(week)
    week   = weekOf(doy)
    doy    = beforeMonth(month) + day 

monthOf(doy)        = min((100*doy)/3034 + 1, 12)
beforeMonth(month)  = 30*(month-1) + (month-1)/3

weekOf(doy)         = (doy-1)/7 + 1
beforeWeek(week)    = 7*(week-1)



--------------------------------------------------------------
-- 
--------------------------------------------------------------
beforeMonthdate(year,month,day) 
  = beforeYear(year) + beforeMonth(month) + (day - 1) 

beforeWeekdate( year, week, wday ) 
  = beforeYear(year) + beforeWeek(week) + (wday - 1)

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
     day         = doy - beforeMonth(month)
     month       = monthOf(doy)
     (year,doy)  = doydateOf(days)  


-- weekdateOf(days) = weekdate(monthdateOf(days))

weekdateOf(days) = (year,week,wday)
   where
     wday        = doy - beforeWeek(week)
     week        = weekOf(doy)
     (year,doy)  = doydateOf(days)
     
doydateOf(days) = (year,doy)
  where
     doy     = days - beforeYear(year) + 1
     year    = if(days >= beforeYear(approx+1)) then approx+1 else approx
     approx  = estimateYear(days-3) -- gyearOf(days-3) 

estimateYear(days)
  = 1 + era*400 + ((100*doe) / 36525)  -- or (10000*doe)/3652425
  where 
     era  = days / 146097
     doe  = days % 146097
    



gyearOf(days) 
  = 1 + (days - leapdays)/365
  where
    leapdays  = doe/1460 - doe/36524 + doe/146096
    doe       = days + 306


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

edivmod :: Integer -> Integer -> (Integer,Integer)
edivmod i d = (ediv i d, emod i d)
-}