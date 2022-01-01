module bayes

/* Goal: Bayesian Statistical Modelling
     Input data: x   y
                 1   1.1
                 2   1.9
                 3   2.7
     `Bayesian homogenous linear regression'

     regression: measure/distribution
                 over functions f : R -> R

     homogeneous linear:
                 assume f(x) = a*x

     Bayesian:   start off with a prior
                 distribution over R -> R,
                e.g.: a ~ Normal(0, 2)
*/

import std/num/double
import std/num/ddouble  // 128-bit double-double
import std/num/random

// --------------------------------------------------------------------
// Non-negative reals represented as a 128-bit `:ddouble` for precision.
// --------------------------------------------------------------------
alias rplus = ddouble

fun rplus(d : double) = d.ddouble
fun rplus(i : int) = i.ddouble

fun list-join(len : int, elem) {
  val xs = list(1,len,elem)
  "[" ++ xs.join(",") ++ "]"
}

fun plot( f : (double -> double) ) : string {
  val lft = 0.0
  val rgt = 4.0
  val steps = 100
  val stp = (rgt - lft) / steps.double
  list-join(steps) fn(i:int) {
    val x = lft + (stp * i.double)
    val y = f( x )
    "[" ++ x.show(6) ++ "," ++ y.show(6) ++ "]"
  }
}

fun example-plot() {
  plot( fn(x){ 2.0 * x } )
}

// --------------------------------------------------------------------
// Effects for Baysian inference.
// `sample` is a `fun` and does not change control flow (i.e. is tail-resumptive)
// `score` is `control` and can use `resume` as a first-class function.
// --------------------------------------------------------------------
effect fun sample() : double
effect control score( s : rplus ) : ()

fun random-sampler(action : () -> <sample|e> a) : <ndet|e> a {
  with fun sample() { srandom-double() }
  mask<ndet>(action)  // mask out `ndet` from the `action` effect
}

/*--------------------------------------------------------------------
  Use Box-Muller to get a normal distribution:
--------------------------------------------------------------------*/

// [Box-Muller](https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform) transform.
fun box-muller(u1 :double, u2 :double) {
  sqrt(-2.0 * log(u1)) * cos(dbl-twopi * u2)
}

fun standard-normal() : <sample> double {
  box-muller(sample(),sample())
}

fun normal(mean, sdv ) {
  mean + standard-normal() * sdv
}

/*--------------------------------------------------------------------
  Statistical modelling
--------------------------------------------------------------------*/
alias model<a>   = () -> <sample,score> a
alias regression = model<double -> double>

val homogeneous-linear : regression = fn() {
  val a = normal(0.0,2.0)
  (fn(x){ a * x })
}

val linear : regression = fn() {
  val a = normal(0.0,2.0)
  val b = normal(0.0,2.0)
  (fn(x){ a * x + b })
}

val homogeneous-cubic : regression = fn() {
  val a = normal(0.0,2.0)
  val b = normal(0.0,2.0)
  val c = normal(0.0,2.0)
  (fn(x){ a * x * x + b * x + c })
}

fun plot-reg() {
  list-join(50) fn(i) {
    with random-sampler
    with fun score(_s) { () }
    plot( homogeneous-linear() )
  }
}

/*--------------------------------------------------------------------

--------------------------------------------------------------------*/
fun normal-pdf(mean : double, sdv : double, x : double) : rplus {
  rplus( exp( ~sqr(x - mean) / (2.0*sqr(sdv)) )
            / (sdv * sqrt(dbl-twopi)) )
}

val fitted-specific : regression = fn() {
  val a = normal(0.0,2.0)
  fun f(x) { a * x }
  normal-pdf( f(1.0), 0.25, 1.1).score
  normal-pdf( f(2.0), 0.25, 1.9).score
  normal-pdf( f(3.0), 0.25, 2.7).score
  f
}

// More generally
val dataset = [(1.0, 1.1), (2.0, 1.9), (3.0, 2.7)]

fun fit(reg : regression, data : list<(double,double)>) : regression = fn(){
  val f : double -> double = reg()
  data.foreach fn(xy){
    val (x,y) = xy  // or `xy.fst`, `xy.snd `
    normal-pdf( f(x), 0.25, y ).score
  }
  f
}

val fitted : regression = fit(linear,dataset)


/*--------------------------------------------------------------------

--------------------------------------------------------------------*/

fun weighted( action : () -> <score|e> a) : e (rplus, a) {
  var w : rplus  := one
  val x = with fun score(s) { w := w*s } in action()
  (w,x)
}

fun plot-weighted-fitted() {
  list-join(15) fn(i) {
    with return(x:(rplus,string)) { x.snd }
    with random-sampler
    with weighted
    fitted().plot

  }
}

/*--------------------------------------------------------------------
  importance sampling
--------------------------------------------------------------------*/

alias list-dist<a> = list<(rplus,a)>

fun populate(k:int, model: model<a> ) : sample list-dist<a> {
  list(1,k) fn(i){
    with weighted
    score(one / k.rplus)
    model()
  }
}

fun sum-weights( ld : list-dist<a> ) : rplus {
  ld.foldl(zero, fn(acc,wx){ acc + wx.fst })
}

fun normalise(pop : list-dist<a>) : list-dist<a> {
  val total = pop.sum-weights
  pop.map(fn(wx){ (wx.fst / total, wx.snd) })
}

fun weighted-choice( ld : list-dist<a>, default : a ) : <sample> a {
  val total = ld.sum-weights.double
  val total-fuel = total * sample()
  fun pick(xs,fuel) {
    match(xs) {
      Cons((w:rplus,x),xx) {
        val fuel1 = fuel - w.double
        if (fuel1.is-pos) then xx.pick(fuel1) else x
      }
      Nil -> default
    }
  }
  ld.pick(total-fuel)
}


fun plot-population(histogram : list<(rplus,double -> double)>) : <sample> string {
  val negligible = histogram.filter( fn(wx){ wx.fst.double < 0.00001 }).length
  list-join(15) fn(i){
    val f = weighted-choice(histogram,id)
    plot(f)
  }
}

fun plot-importance-sampling() : ndet string {
  val particle-count = 1000
  with random-sampler
  populate(particle-count,fitted).normalise.plot-population
}

/*--------------------------------------------------------------------
  resampling
--------------------------------------------------------------------*/
fun resample(ld : list-dist<a>, default : a ) : sample list-dist<a> {
  val n = ld.length
  val total = ld.sum-weights
  fun resample-model() {
    score(total)
    ld.weighted-choice(default)  // guarantee it doesn't score.
  }
  populate(n,resample-model)
}

alias thunk<a> = () -> <score,sample> a  // todo: make it polymorphic in `e`

// Break on score-ing
fun yield-on-score(action : () -> <score,sample> a) : <score,sample> thunk<a> {
  with control score(w) {
    score(w)
    ({ resume(())() })   // or (fn(){ resume(()) })
  }
  val x = action()
  ({ x })
}

fun advance(thunks : list-dist<thunk<a>> ) : sample list-dist<thunk<a>> {
  thunks.map fn(wt) {
    val (w,t) = wt
    with weighted
    score(w)
    with yield-on-score
    t()
  }
}

fun trun( thunks : list-dist<thunk<a>> ) : sample list-dist<a> {
  thunks.map fn(wt) {
    val (w,t) = wt
    with weighted
    score(w)
    t()
  }
}

fun score-fitted() : <score,sample> thunk<double -> double> {
  with yield-on-score
  fitted()
}

fun plot-smc() : <ndet> string {
  with random-sampler
  val particle-count = 1000
  val steps     = 4
  val pop       = populate(particle-count,score-fitted)
  val thunks    = list(1,steps).foldl(pop, fn(p,i) { p.resample(fn(){id}).advance } )
  val histogram = thunks.trun
  histogram.plot-population()
}

/*
  Summary:

   + With enough glue,
      <<insert your language>>
     can support:
        probabilistic programming
        statistical modelling
        statistical machine learning

   + Work in progress!
     TODO: Trace MCMC ---> rmsmc, smc^2,...

   + (Attempted) simplification of
           monad-bayes
       (this, but with monads)
   + TODO: correctness via reasoning in MEGA

*/


fun browser-main() : <ndet> string {
  // example-plot()
  // plot-reg()
  // plot-weighted-fitted()
  // plot-importance-sampling()
  plot-smc()
}
