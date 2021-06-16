// Comma Separated Values
// Conversion to CSV as in https://datatracker.ietf.org/doc/html/rfc4180
// Conversion from CSV as its inverse (but not compatible with other valid encodings)

module csv

import std/text/parse
import std/num/double

alias csv = string

// Converting to CSV

fun to-csv(d : double) : csv {
    d.show
}

fun to-csv(i : int) : csv {
    i.show
}

fun to-csv(s : string) : csv {
    "\"" ++ s.split("\"").join("\"\"") ++ "\""
}

fun to-csv-row(xs : list<csv>) : csv {
    xs.join(",")
}

fun to-csv-rows(xs : list<csv>) : csv {
    xs.join("\r\n")
}

// Converting from CSV

fun csv-double() : parse double {
    fun int-length(n : int) : int {
        if(n <= 0) { 0 }
        else { 1 + int-length(unsafe-decreasing(n / 10)) }
    }
    val before-dot = pint()
    val after-dot = choose([fn() { char('.'); pint() }, fn() { 0 }])
    encode(before-dot, 0) + encode(after-dot, ~ int-length(after-dot))
}

fun csv-int() : parse int {
    pint()
}

fun any-char() : parse char {
    satisfy-fail("") fn(slice) {
        match(slice.next) {
            Just((c, rest)) -> Just((c, rest))
            Nothing -> Nothing
        }
    }
}

fun checkContinue() {
    val next = satisfy fn(slice) {
        match(slice.next) {
            Just((c, rest)) | c == '\"' -> Just (('\"', rest))
            _ -> Nothing
        }
    }
    match(next) {
        Just(c) -> Cons(c, getString());
        Nothing -> Nil
    }
}
    
fun getString() {
    val c = any-char()
    if(c == '\"') {
        checkContinue()
    } else {
        Cons(c, getString())
    }
}

fun csv-string() : <div, parse> string {
    char('\"')
    getString().string
}

fun csv-rows(line : () -> parse a) : parse list<a> {
    many fn() {
        val l = line()
        char('\r'); char('\n')
        l
    }
}

fun csv-rows1(line : () -> parse a) : parse list<a> {
    many1 fn() {
        val l = line()
        char('\r'); char('\n')
        l
    }
}

fun csv-row(a : () -> parse a) : parse list<a> {
    val a1 = a()
    val as_ = many fn() {
        char(',')
        a()
    }
    Cons(a1, as_)
}

fun csv-row(a : () -> parse a, b : () -> parse b) : parse (a, list<b>) {
    val a1 = a()
    val bs = many1 fn() {
        char(',')
        b()
    }
    (a1, bs)
}

fun csv-row(a : () -> parse a, b : () -> parse b, c : () -> parse c) : parse (a, b, list<c>) {
    val a1 = a()
    char(',')
    val b1 = b()
    val cs = many1 fn() {
        char(',')
        c()
    }
    (a1, b1, cs)
}