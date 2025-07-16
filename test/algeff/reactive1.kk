named effect rref<a>
  fun emit(a: a): ()
  ctl watch_<b>(p: provider<b>, immediate: bool): b
  ctl yield(): ()

fun watch(r: ev<rref<a>>, p: provider<b>, immediate: bool = True): io b
  r.watch_(p, immediate)

div type provider<a>
  Provider(
    listeners: ref<global, list<(a) -> io ()>>,
    current: () -> io a
  )

fun mk-provider(reactor-fn: ev<rref<a>> -> io a, show: (a) -> io string, name="Provider"): io provider<a>
  val current-state : some<a> ref<global, maybe<a>> = ref(Nothing)
  val continuex: ref<global, () -> io ()> = ref(fn(){})
  val do-init = fn(rref)
      rref.yield()
      val result = reactor-fn(rref)
      rref.emit(result)
      ()
  val provider = Provider(ref([]), fn(){
    match !current-state
      Nothing ->
        (!continuex)()
        match !current-state
          Nothing -> throw("Not initialize")
          Just(x) -> x
      Just(k) ->
        k
    })
  named handle(do-init)
    fun emit(new-value)
      current-state := Just(new-value)
      (!provider.listeners).foreach fn(listener)
        listener(new-value)
    raw ctl yield()
      continuex := (fn() {rcontext.resume(())})
    raw ctl watch_(other-provider, trigger-immediately)
      other-provider.listeners.set(Cons(fn(b) {rcontext.resume(b)}, !(other-provider.listeners)))
      if trigger-immediately then
        (other-provider.current)()
        ()
  provider

fun main()
  val result = try
    val o-provider = mk-provider(
      fn(r: ev<rref<int>>) {
        r.emit(1)
        r.emit(2)
        0
      },
      fn (x: int) x.show,
      "o"
    )
    val s-provider = mk-provider(
      fn(r: ev<rref<int>>){
        trace("Hi")
        r.emit(100)
        val x = r.watch(o-provider, True)
        trace("X: " ++ x.show)
        1
      },
      fn(x:int) x.show,
      "s"
    );
    (s-provider.current)()
  match result
    Ok(x) -> x.show
    Error(e) -> e.show
