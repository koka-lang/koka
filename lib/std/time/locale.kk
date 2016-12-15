/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Localization of time formats.
module std/time/locale

// -----------------------------------------------------------
// Time locale
// -----------------------------------------------------------

// The `:time-locale` describes time and date formats for a specific locale.
public struct time-locale(
  // Language name, for example `en` or `fr-ca`.                          
  lang-name       : string,                            
  day-names       : list<string>,
  month-names     : list<string>,
  month-names-short: list<string> = month-names.map(fun(m) { m.first(3).string }),
  day-names-short : list<string>  = day-names.map(fun(m) { m.first(3).string }),
  day-names-min   : list<string>  = day-names.map(fun(m) { m.first(2).string }),
  format-t        : string = "HH:mm",
  format-tt       : string = "HH:mm:ss",
  format-l        : string = "YYYY-MM-DD",
  format-ll       : string = "D MMMM YYYY",
  format-lll      : string = "D MMMM YYYY t",
  format-llll     : string = "dddd D MMMM YYYY t"
)

// English time locale (`en`)
public val time-locale-en : time-locale = Time-locale( "en",
  ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],
  ["January","February","March","April","May","June","July","August","September","October","November","December"],
  format-t    = "h:mmaa",
  format-tt   = "h:mm:ssaa",
  format-l    = "MM/DD/YYYY",
  format-llll = "dddd, D MMMM YYYY t"
)

// ISO English time locale (`en-iso`). Uses English names for
// months and days but displays numeric dates and times using unambigious ISO format. 
public val time-locale-en-iso : time-locale = Time-locale( "en-iso",
  day-names   = time-locale-en.day-names,
  month-names = time-locale-en.month-names,
  format-lll  = "D MMMM YYYY, tt",
  format-llll = "dddd, D MMMM YYYY, tt"
)

// French time locale (`fr`)
public val time-locale-fr : time-locale = Time-locale( "fr",                                                      
  ["lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"],
  ["janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"],
  ["janv.","févr.","mars","avr.","mai","juin","juil.","août","sept.","oct.","nov.","déc."],
  ["lun.","mar.","mer.","jeu.","ven.","sam.","dim."],
  ["Lu","Ma","Me","Je","Ve","Sa","Di"],
  format-l    = "DD/MM/YYYY"
)

// German time locale (`de`)
public val time-locale-de : time-locale = Time-locale( "de",
  ["Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"],
  ["Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"],
  ["Jan.","Febr.","Mrz.","Apr.","Mai","Jun.","Jul.","Aug.","Sept.","Okt.","Nov.","Dez."],
  ["Mo.","Di.","Mi.","Do.","Fr.","Sa.","So."],
  ["Mo","Di","Mi","Do","Fr","Sa","So"],
  format-l    = "DD.MM.YYYY",
  format-ll   = "D. MMMM YYYY",
  format-lll  = "D. MMMM YYYY t",
  format-llll = "dddd, D. MMMM YYYY t"
)

// Spanish time locale (`es`)
public val time-locale-es : time-locale = Time-locale( "es",
  ["lunes","martes","miércoles","jueves","viernes","sábado","domingo"],
  ["enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"],
  format-t    = "H:mm",
  format-tt   = "H:mm:ss",
  format-l    = "DD/MM/YYYY",
  format-ll   = "D 'de' MMMM 'de' YYYY",
  format-lll  = "D 'de' MMMM 'de' YYYY H:mm",
  format-llll = "dddd, D 'de' MMMM 'de' YYYY H:mm"
)

// Dutch time locale (`nl`)
public val time-locale-nl : time-locale = Time-locale( "nl",
  ["maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"],
  ["januari","februari","maart","april","mei","juni","juli","augustus","september","oktober","november","december"],
  ["jan","feb","mrt","apr","mei","jun","jul","aug","sep","okt","nov","dec"],
  ["ma.","di.","wo.","do.","vr.","za.","zo."],
  ["Ma","Di","Wo","Do","Vr","Za","Zo"],
  format-l    = "DD-MM-YYYY"
)

val locales = [
  // from most specific (``en-GB`` to least specific ``en``)
  time-locale-en-iso,
  time-locale-de,time-locale-en,
  time-locale-es,time-locale-fr,
  time-locale-nl
]

// Return a builtin locale given a locale string (e.g. ``en-GB``, ``es``); 
// Use `time-locale-iso` if no particular match is found.
// Supports ``de``,``en``,``es``,``fr``,``nl`` and ``en-iso``.
public fun get-time-locale( locale : string ) : time-locale {
  locales.find(fun(l) { locale.starts-with(l.lang-name).bool }).default(time-locale-en-iso)
}