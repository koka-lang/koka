module plot

import std/os/path
import std/os/file

// scatter(x, y, Green)
// xlabel("x")
// ylabel("y")
// plot(x, t, Red)
// show()

// show function for list<float64>
pub fun show(xs: list<float64>): <> string
  xs.show-list(fn(x) { show(x) })

//-------------------------------------------------------------------------------------------------
// Color of line and dot
//-------------------------------------------------------------------------------------------------

pub type color 
  Default-color
  Red
  Green
  Blue
  Yellow
  // Rgb16(c: string)

fun py-color(c: color): <> maybe<string> 
  match c
      Default-color -> Nothing
      Red -> Just("\'r\'")
      Green -> Just("\'g\'")
      Blue -> Just("\'b\'")
      Yellow -> Just("\'y\'")

//-------------------------------------------------------------------------------------------------
// Line style
//-------------------------------------------------------------------------------------------------

// type line-style {
//     ...
//  }

//-------------------------------------------------------------------------------------------------
// Marker style
//-------------------------------------------------------------------------------------------------

pub type marker
  Default-marker
  Plus
  Star
  Circle

fun py-marker(m: marker): <> maybe<string>
  match m
    Default-marker -> Nothing
    Plus -> Just("\'+\'")
    Star -> Just("\'*\'")
    Circle -> Just("\'o\'")


//-------------------------------------------------------------------------------------------------
// Python Variavles
//-------------------------------------------------------------------------------------------------
struct pyvar
  tag: int

fun get-var(pv: pyvar): <> string 
  "x" ++ show(pv.tag)


//-------------------------------------------------------------------------------------------------
// matplotlib methods
//-------------------------------------------------------------------------------------------------

pub effect kkplot 
  fun py-list(xs: list<float64>): pyvar
  fun kkshow(x: () = ()): ()
  fun kkplot(x: pyvar, y: pyvar, c: color = Default-color): ()
  fun kkscatter(x: pyvar, y: pyvar, m: marker = Default-marker): ()



//-------------------------------------------------------------------------------------------------
// pyplot handler
//-------------------------------------------------------------------------------------------------
pub fun pyplot(p-py: path, f: () -> <kkplot, exn, fsys | e> ()): <exn, fsys | e> () {
    val p-json = path("data.json")
    // initial program
    var content-py := "#%%\nimport numpy as np\nimport matplotlib.pyplot as plt\nimport json\n\n"
    content-py := content-py ++ "with open(\"" ++ p-json.string ++ "\") as f:\n"
    content-py := content-py ++ "   df = json.load(f)\n"

    var content-json := "{"
    var tag := 0
    with handler {
        return(x) {
            content-json := content-json.trim-right(",\n") ++ "}"
            write-text-file(p-py, content-py)
            write-text-file(p-py.dirname.path / p-json, content-json)
        }
        fun kkshow(x: ()) {
            content-py  := content-py ++ "plt.show()\n"
        }
        fun py-list(xs) {
            val x = Pyvar(tag)

            // in json file
            content-json := content-json ++ "\"" ++ x.get-var() ++ "\":" ++ show(xs) ++ ",\n" 

            // in python file
            content-py := content-py ++ x.get-var ++ " = df[\"" ++ x.get-var ++ "\"]\n"

            // increment unique tag
            tag := tag + 1
            x
        }
        fun kkplot(x, y, c: color) {
            val x-pyvar = x.get-var()
            val y-pyvar = y.get-var()
            val c-py = c.py-color()

            // plt.plot(x-var, y-var)
            content-py := content-py ++ "plt.plot(" ++ x-pyvar ++ ", " ++ y-pyvar
            match(c-py) {
                Nothing -> content-py := content-py ++ ")\n"
                Just(xc) -> content-py := content-py ++ ", color=" ++ xc ++ ")\n"
            }
        }
        fun kkscatter(x, y, m: marker) {
            val x-pyvar = x.get-var()
            val y-pyvar = y.get-var()
            val m-py = m.py-marker()

            // plt.scatter(x-var, y-var)
            content-py := content-py ++ "plt.scatter(" ++ x-pyvar ++ ", " ++ y-pyvar

            match(m-py) {
                Nothing -> content-py := content-py ++ ")\n"
                Just(xm) -> content-py := content-py ++ ", marker=" ++ xm ++ ")\n"
            }
        } 
    } 
    f()
}
