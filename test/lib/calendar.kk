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

// ----------------------------------------------------
// Generic solar calendar construction
// ----------------------------------------------------

fun solar-calendar( name : string, long-name: string,
                    days-before-year : (year:int) -> int,
                    days-to-yeardoy  : (days:int) -> (int,int),
                    days-before-month: (year:int, month:int) -> int,
                    days-to-month    : (year:int, doy: int ) -> int,
                    month-prefix  : string = "",
                    show-era      : (date)->string = fun(d){ "" },
                    ts            : timescale = ts-utc,
                    epoch-shift   : int = 730485  ) 
{
  fun date-to-days( d : date ) : int {
    days-before-year( d.year ) + days-before-month( d.year, d.month ) 
     + d.day.dec - epoch-shift
  }

  fun days-to-date( days0 : int ) : date {
    val days       = days0 + epoch-shift
    val (year,doy) = days-to-yeardoy( days )
    val month      = days-to-month(year, doy)
    val day        = (doy - days-before-month(year, month)) + 1
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
                    estimate-year    : (days:int) -> (int,int),
                    days-before-month: (year:int, month:int) -> int,
                    days-to-month    : (year:int, doy: int ) -> int,
                    month-prefix  : string = "",
                    show-era      : (date)->string = fun(d){ "" },
                    ts            : timescale = ts-utc,
                    epoch-shift   : int = 730485  ) 
{
  fun days-to-yeardoy(days:int) : (int,int) {
    val (approx,safe) = estimate-year(days)
    val doy1 = days - days-before-year(approx)
    if (doy1 <= safe) then return (approx,doy1)
    val doy2 = days - days-before-year(approx.inc)
    if (doy2 < 0) then (approx,doy1) else (approx.inc, doy2)
  }

  solar-calendar(name,long-name,
    days-before-year, days-to-yeardoy, 
    days-before-month, days-to-month,
    month-prefix, show-era, ts, epoch-shift
  )
}


// ----------------------------------------------------
// ISO calendar
// ----------------------------------------------------

val xcal-iso = solar-ecalendar(
  "XISO", "XISO", 
  iso-days-before-year, iso-estimate-year, 
  iso-days-before-month, iso-doy-to-month
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
  (1 + 365*year) + leapdays
}

fun iso-estimate-year( days : int ) : (int,int) {
  val (era,yoe) = divmod(days,146097)
  (400*era + (100*yoe)/36525, 363)
}

// ----------------------------------------------------
// ISO week calendar
// ----------------------------------------------------

val xcal-isow = solar-ecalendar(
  "XISOW", "XISO Week", 
  isow-days-before-year, isow-estimate-year, 
  isow-days-before-month, isow-doy-to-month
)

fun isow-days-before-month( year : int, month : int ) : int { 
  7*(month - 1)
}

fun isow-doy-to-month( year : int, doy : int ) : int {
  doy/7 + 1
}

fun isow-days-before-year( year : int ) {
  val gdays   = iso-days-before-year(year)
  val weekday = (gdays - 2)%7  // Monday=0, Saturday=6 (because 0000-01-01 is a Saturday)
  val adjust  = if (weekday <= 3) then (0 - weekday) else (7 - weekday)
  gdays + adjust
}

fun isow-estimate-year( days : int ) : (int,int) {
  iso-estimate-year( days - 3 )  // because we stick out at most 3 days into the next year
}


// ----------------------------------------------------
// Coptic calendar
// ----------------------------------------------------

val xcal-coptic = solar-calendar(
  "XJ", "coptic", 
  coptic-days-before-year, coptic-days-to-yeardoy, 
  coptic-days-before-month, coptic-doy-to-month,
  epoch-shift = 626515 + 365 - 3
)

fun coptic-days-before-month( year : int, month : int ) : int { 
  30*(month - 1)
}

fun coptic-doy-to-month( year : int, doy : int ) : int {
  doy/30 + 1
}

fun coptic-days-before-year( year : int ) {
  365*year + year/4
}

fun coptic-days-to-yeardoy( days : int ) : (int,int) {
  val (era,doe) = divmod(days,1461)
  val yoe = min( doe/365, 3)
  (4*era + yoe, doe - 365*yoe)
}

val xcal-ethiopian = solar-calendar(
  "XE", "Ethiopian", 
  coptic-days-before-year, coptic-days-to-yeardoy, 
  coptic-days-before-month, coptic-doy-to-month,
  epoch-shift = 727324 + 365
)

// ----------------------------------------------------
// Julian calendar
// ----------------------------------------------------

val xcal-julian = solar-ecalendar(
  "Xjulian", "Xjulian", 
  julian-days-before-year, julian-estimate-year, 
  julian-days-before-month, julian-doy-to-month,
  epoch-shift = 730121 + 366
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
  (1 + 365*year) + leapdays
}

fun julian-estimate-year( days : int ) : (int,int) {
  val (era,yoe) = divmod(days,1461)
  (4*era + yoe/366, 364)
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
  /*
  val idate = test-day-cal( day, cal-iso )
  val xidate = test-day-cal( day, xcal-iso )
  check( "equal iso date: " + idate.show, idate.show, xidate.show)
  val wdate  = test-day-cal( day, cal-iso-week )
  val xwdate = test-day-cal( day, xcal-isow )
  check( "equal iso week date: " + wdate.show, wdate.show, xwdate.show)
  */
  val jdate  = test-day-cal( day, cal-julian )
  val xjdate = test-day-cal( day, xcal-julian )
  check( "equal julian date: " + jdate.show, jdate.show, xjdate.show)
  // val edate  = test-day-cal( day, cal-ethiopian )
  val xedate = test-day-cal( day, xcal-ethiopian )
  // check( "equal ethiopian date: " + edate.show, edate.show, xedate.show)
  // val cdate = test-day-cal(day, cal-coptic )
  val xcdate = test-day-cal(day, xcal-coptic)
  // println("coptic: " + cdate.show + ", " + xcdate.show)
  //check("equal coptic date: " + cdate.show, cdate.show, xcdate.show )
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
    println("year: " + y.show)
    val base = y*366
    for(0,365) fun(day) { action(base + day)}
  }
}


fun main() {
  test()
}