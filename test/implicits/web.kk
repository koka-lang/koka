type method {
  GET; POST
}

fun show(m : method) {
  match (m) {
    GET -> "GET"
    POST -> "POST"
  }
}

effect request {
  val requestUrl : string
  val method : method
}

effect context {
  val port : int
}

effect routes {
  fun registerRoute(route : string, meth: method, action : () -> <context, request> string): ()
}

fun get(route, action) { registerRoute(route, GET, action) }
fun post(route, action) { registerRoute(route, POST, action) }


fun server() {

  get("/hello") {
    "hello"
  }

  get("/world") {
    "world"
  }

  post("/form/*") {
    if (requestUrl == "/form/1") {
      "first form"
    } else {
      "second form"
    }
  }

}


fun serverInfo(action : () -> routes ()) : list<string> {
  handle(action)(rs = []) {
    return a -> rs
    control registerRoute(r, _m, _a) -> resume((), [r] + rs)
  }
}
