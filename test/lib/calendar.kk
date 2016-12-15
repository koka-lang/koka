public module calendar

public import std/num/fixed
public import std/xtime


// ----------------------------------------------------
// Make it easy to work with days since epoch
// ----------------------------------------------------

val mjd-y2000 = 51544

fun date-at-days( days : int, cal : calendar ) : date {
  instant-at-mjd( fixed(days + mjd-y2000), ts-utc ).time(cal=cal).date
}

fun days-at-date( date : date, cal : calendar ) : int {
  time(date, cal=cal).mjd.round - mjd-y2000
}

fun xiso-calendar( ts : timescale, name : string = ts.name, long-name : string = name ) : calendar {
  xcal-iso( name=name, long-name=long-name, timescale=ts )
}



// ----------------------------------------------------
// Generic solar calendar construction
// ----------------------------------------------------

fun solar-calendar( name : string, long-name: string,
                    days-before-year : (year:int) -> int,
                    days-to-yeardoy  : (days:int) -> (int,int),
                    days-before-month: (year:int, month:int) -> int,
                    days-to-month    : (year:int, doy: int ) -> int,
                    epoch-shift   : int = 730119,
                    has-year-zero?: bool = True,
                    month-prefix  : string = "",
                    show-era      : (date)->string = fun(d){ "" },
                    ts            : timescale = ts-utc ) : calendar 
{
  fun date-to-days( d : date ) : int {
    val y = if (has-year-zero? || d.year.pos?) then d.year else d.year.inc
    days-before-year( y ) + days-before-month( y, d.month ) 
     + d.day.dec - epoch-shift
  }

  fun days-to-date( days0 : int ) : date {
    val days    = days0 + epoch-shift
    val (y,doy) = days-to-yeardoy( days )
    val month   = days-to-month(y, doy)
    val day     = (doy - days-before-month(y, month)) + 1
    val year    = if (has-year-zero? || y.pos?) then y else y.dec
    Date(year,month,day)
  }

  earth-calendar( 
    name, long-name, 
    days-to-date, date-to-days, 
    month-prefix, show-era, ts 
  )
}

fun solar-ecalendar( name : string, long-name: string,
                    days-before-year : (year:int) -> int,
                    estimate-year    : (days:int) -> (int,maybe<int>),
                    days-before-month: (year:int, month:int) -> int,
                    days-to-month    : (year:int, doy: int ) -> int,
                      epoch-shift   : int = 730119,
                    has-year-zero?: bool = True,
                    month-prefix  : string = "",
                    show-era      : (date)->string = fun(d){ "" },
                    ts            : timescale = ts-utc )  : calendar
{
  fun days-to-yeardoy(days:int) : (int,int) {
    val (approx,safe) = estimate-year(days)
    val doy1 = days - days-before-year(approx)
    if (doy1 <= safe.default(0)) then return (approx,doy1)
    val doy2 = days - days-before-year(approx.inc)
    if (doy2 < 0) then (approx,doy1) else (approx.inc, doy2)
  }

  solar-calendar(name,long-name,
    days-before-year, days-to-yeardoy, 
    days-before-month, days-to-month,
    epoch-shift, has-year-zero?, month-prefix, show-era, ts
  )
}


// ----------------------------------------------------
// ISO calendar
// ----------------------------------------------------

val xcal-iso = solar-ecalendar(
  "x", "ISO", 
  iso-days-before-year, iso-estimate-year, 
  iso-days-before-month, iso-doy-to-month
)

val xcal-gregorian = solar-ecalendar(
  "xGC", "Gregorian", 
  iso-days-before-year, iso-estimate-year, 
  iso-days-before-month, iso-doy-to-month,
  has-year-zero? = False,
  show-era = fun(d:date){ if (d.year.neg?) then "BCE" else "CE" }
)

fun iso-days-before-month( year : int, month : int ) : int { 
  val adj = iso-adjust( month<=2, year )
  (367*month - 362)/12 - adj
}

fun iso-doy-to-month( year : int, doy : int ) : int {
  val adj = iso-adjust( doy<=58,  year)
  (12*(doy + adj) + 373)/367
}

fun iso-adjust( before-march? : bool, year : int ) : int {
  if (before-march?) then 0 elif (iso-is-leap?(year)) then 1 else 2
}
    
fun iso-is-leap?( year : int ) {
  (year%4 == 0) && (year%100 != 0 || year%400 == 0)
}

fun iso-days-before-year( year : int ) {
  val y = year.dec
  val leapdays = (y/4 - y/100) + y/400
  365*y + leapdays
}

fun iso-estimate-year( days : int ) : (int,maybe<int>) {
  val (era,yoe) = divmod(days,146097)
  (1 + 400*era + (100*yoe)/36525, Just(363))
}

// ----------------------------------------------------
// ISO week calendar
// ----------------------------------------------------

val xcal-iso-week = solar-ecalendar(
  "xIW", "ISO Week", 
  isow-days-before-year, isow-estimate-year, 
  isow-days-before-month, isow-doy-to-month,
  month-prefix = "W"  
)

fun isow-days-before-month( year : int, month : int ) : int { 
  7*(month - 1)
}

fun isow-doy-to-month( year : int, doy : int ) : int {
  doy/7 + 1
}

fun isow-days-before-year( year : int ) {
  val gdays   = iso-days-before-year(year)
  val weekday = gdays%7  // Monday=0, Saturday=6 (because 0001-01-01 is a Monday)
  val adjust  = if (weekday <= 3) then (0 - weekday) else (7 - weekday)
  gdays + adjust
}

fun isow-estimate-year( days : int ) : (int,maybe<int>) {
  iso-estimate-year( days - 3 )  // because we stick out at most 3 days into the next year
}

// ----------------------------------------------------
// ISO month calendar
// ----------------------------------------------------

val xcal-iso-month = solar-ecalendar(
  "xIM", "ISO Month", 
  isow-days-before-year, isow-estimate-year, 
  isom-days-before-month, isom-doy-to-month,
  month-prefix = "M"  
)

fun isom-days-before-month( year : int, month : int ) : int { 
  30*month.dec + month.dec/3
}

fun isom-doy-to-month( year : int, doy : int ) : int {
  val m = (100*doy.inc)/3034   
  min(m.inc,12)
}


// ----------------------------------------------------
// Coptic calendar
// ----------------------------------------------------

val xcal-coptic = solar-ecalendar(
  "xCC", "Coptic", 
  coptic-days-before-year, coptic-estimate-year, // coptic-days-to-yeardoy, 
  coptic-days-before-month, coptic-doy-to-month,
  epoch-shift = 626515,  // 2001-01-01 - 284-08-29 Julian
  show-era = fun(d:date){ if (d.year.pos?) then "A.M." else "" }  // Anno Martyrum
)

fun coptic-days-before-month( year : int, month : int ) : int { 
  30*(month - 1)
}

fun coptic-doy-to-month( year : int, doy : int ) : int {
  doy/30 + 1
}   

fun coptic-days-before-year( year : int ) {
  365*year.dec + year/4
}

fun coptic-estimate-year( days : int ) : (int,maybe<int>) {
  val (era,doe) = divmod(days + 365, 1461)
  (4*era + doe/366, Just(364))
}

fun coptic-days-to-yeardoy( days : int ) : (int,int) {
  val (era,doe) = divmod(days + 365, 1461)
  val yoe = min( doe/365, 3)
  (4*era + yoe, doe - 365*yoe)
}

val xcal-ethiopian = solar-calendar(
  "xEC", "Ethiopian", 
  coptic-days-before-year, coptic-days-to-yeardoy, 
  coptic-days-before-month, coptic-doy-to-month,
  epoch-shift = 727324,  // year 0000-01-01 = 0007-08-30 (Julian), year 0001-01-01 = 0008-08-29! (Julian))
  show-era = fun(d:date){ if (d.year.pos?) then "A.M." else "" }  // Anno Martyrum
)

// ----------------------------------------------------
// Julian calendar
// ----------------------------------------------------

val xcal-julian = solar-ecalendar(
  "xJC", "Julian", 
  julian-days-before-year, julian-estimate-year, 
  julian-days-before-month, julian-doy-to-month,
  epoch-shift    = 730121, // 2000-01-01 - 0000-12-30  // 730121 + 366
  has-year-zero? = False,
  show-era       = fun(d:date){ if (d.year.neg?) then "BC" else "AD" }
)

fun julian-days-before-month( year : int, month : int ) : int { 
  val adj = julian-adjust( month<=2, year )
  (367*month - 362)/12 - adj
}

fun julian-doy-to-month( year : int, doy : int ) : int {
  val adj = julian-adjust( doy<=58,  year)
  (12*(doy + adj) + 373)/367
}

fun julian-adjust( before-march? : bool, year : int ) : int {
  if (before-march?) then 0 elif (year%4==0) then 1 else 2
}
    
fun julian-days-before-year( year : int ) {
  val leapdays = year.dec/4
  365*year.dec + leapdays
}

fun julian-estimate-year( days : int ) : (int,maybe<int>) {
  val (era,yoe) = divmod(days + 365, 1461)
  (4*era + yoe/366, Just(364))
}

fun xjulian-gregorian( switch : date = Date(1582,10,15) ) : calendar {
  val switch-days = (xcal-iso.date-to-days)(switch) //+ skip
  earth-calendar( "JG", "Julian-Gregorian",
    fun(days) { julgreg-days-to-date(days,switch-days) },
    fun(d)    { julgreg-date-to-days(d,switch) },
    show-era = fun(d:date) { 
      if (d < switch) {
        if (d.year >= 1500) then "CE (O.S.)" else (cal-julian.show-era)(d)
      }
      else {
        if (d.year <= 1926) then "CE (N.S.)" else (cal-gregorian.show-era)(d)
      }
    }
  )
}

fun julgreg-date-to-days( d : date, switch : date ) : int {
  if (compare(d,switch).lt?) 
    then (xcal-julian.date-to-days)(d)
    else (xcal-iso.date-to-days)(d) 
}

fun julgreg-days-to-date( days : int, switch : int ) : date {
  if (days < switch) 
   then (xcal-julian.days-to-date)(days)
   else (xcal-iso.days-to-date)(days)
}

// ----------------------------------------------------
// Test
// ----------------------------------------------------
fun check( msg : string, expect : string, result : string ) : io () {
  if (expect==result) then () // println( msg + ": ok.") 
  else {
    println(msg + ": FAILED:\n expect : " + expect + "\n result: " + result )
    error("failure")
  }
}

fun test-day-cal( day : int, cal : calendar ) : io date {
  val date1 = date-at-days( day, cal )
  val days1 = days-at-date( date1, cal )
  val date2 = date-at-days( days1, cal )
  val days2 = days-at-date( date2, cal )
  check( "inverse days1: " + cal.name.show + ", " + date1.show, day.show, days1.show )
  check( "inverse days2: " + day.show, day.show, days2.show )
  check( "inverse date: " + date1.show, date1.show, date2.show )
  date1
}

fun test-day( day : int ) {
  val idate = test-day-cal( day, cal-iso )
  val xidate = test-day-cal( day, xcal-iso )
  //println(" iso date: " + idate.show + ", " + xidate.show )
  check( "equal iso date: " + idate.show, idate.show, xidate.show)
  val wdate  = test-day-cal( day, cal-iso-week )
  val xwdate = test-day-cal( day, xcal-iso-week )
  check( "equal iso week date: " + wdate.show, wdate.show, xwdate.show)
  val mdate  = test-day-cal( day, cal-iso-month )
  val xmdate = test-day-cal( day, xcal-iso-month )
  check( "equal iso month date: " + mdate.show, mdate.show, xmdate.show)
  val jdate  = test-day-cal( day, cal-julian )
  val xjdate = test-day-cal( day, xcal-julian )
  check( "equal julian date: " + jdate.show, jdate.show, xjdate.show)
  val cdate = test-day-cal(day, cal-coptic )
  val xcdate = test-day-cal(day, xcal-coptic)
  // println("coptic: " + cdate.show + ", " + xcdate.show)
  check("equal coptic date: " + cdate.show, cdate.show, xcdate.show )
  val edate  = test-day-cal( day, cal-ethiopian )
  val xedate = test-day-cal( day, xcal-ethiopian )
  check( "equal ethiopian date: " + edate.show, edate.show, xedate.show)
  ()
}

fun test( years : int = 500 ) {
  for-days( ~years, years ) fun(day){
    test-day(day)    
  }
  println("ok.")
}

fun for-days( ylo : int, yhi : int, action : int -> io () ) : io () {
  println("days in years: " + ylo.show + " up to " + yhi.show)
  for(ylo,yhi) fun(y) {
    val base = y*366 - 730119
    println("year: " + time(2000,1,base).year.show)
    for(0,365) fun(day) { action(base + day)}
  }
}


fun main() {
  test()
}