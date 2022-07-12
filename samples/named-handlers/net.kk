module net

import std/num/float64
import std/num/random
import std/os/path
import std/os/file
import net/tensor
import net/variable
import net/autodiff
import net/layers
import net/plot

//-------------------------------------------------------------------------------------------------
// simple neural net work 1 10 1
// sin curve
//-------------------------------------------------------------------------------------------------


fun mse(x: variable<h>, t: variable<h>): <deriv<variable<h>>, alloc<h>> variable<h> 
  val diff = x - t
  sum(diff * diff) / variable(100.0)


fun sigmoid(x: variable<h>): <deriv<variable<h>>, alloc<h>> variable<h>
  1.0 / (1.0 + exp(0.0 - x))


fun sgd(w: variable<h>, lr: float64): <alloc<h>, read<h>, console> variable<h>
  val t = w.data - lr * (!w.grad)
  variable(t)


// define net
struct net<a::V>
  layer1 : linear<a>
  layer2 : linear<a>
  layer3 : linear<a>


fun model(net: net<variable<h>>, x: variable<h>): <div, exn, alloc<h>, deriv<variable<h>>> variable<h>
  val x1 = net.layer1.linearE(x)
  val x2 = sigmoid(x1)
  val x3 = net.layer2.linearE(x2)
  val x4 = sigmoid(x3)
  val x5 = net.layer3.linearE(x4)
  x5

fun model( net : list<linear<variable<h>>>, x: variable<h>): <pure,alloc<h>,deriv<variable<h>>> variable<h>
  match net.reverse
    Nil -> x
    Cons(l,ls) ->
      val y = ls.foldr(x, fn(layer,y) linearE(layer,y).sigmoid)
      l.linearE(y)

pub fun main() 
  // train data
  val xs = list(0, 99, fn(i:int) { float64(i) / 100.0 - 0.5 })
  val ts = xs.map(fn(x) { sin(2.0 * pi * x) })
  val x-data = variable(tensor(xs).reshapet((100, 1)))
  val t-data = variable(tensor(ts).reshapet((100, 1)) + 0.2 * randn((100, 1)))

  // hyperparameters
  val i = 1
  val h = 3
  val o = 1
  val lr = 0.2
  val iters = 5000

  /*
  with layers

  // define net
  val net = Net(new-linear(i, h),
                new-linear(h, h),
                new-linear(h, o))
  */
  with l1 <- linear(i,h)
  with l2 <- linear(h,h)
  with l3 <- linear(h,o)

  val net = [l1,l2,l3]
  var errs := []

  for(1,iters) fn(cnt:int)
    val loss = backprop{ net.model(x-data).mse(t-data) }

    // train loss
    errs := Cons(loss.data.at(0, 0), errs)

    // print loss
    if (cnt % 100 == 0 || cnt == 1) then
      print("epochs " ++ show(cnt) ++ ": ")
      println(loss)


    // update
    net.foreach( fn(l) optimizer-step(l,lr) )  

  val yv = eval { net.model(x-data) }

  val p = path("./netplot/plot.py")
  pyplot(p) 
    val x-list = xs.py-list
    val t-list = t-data.list.py-list
    val y-list = yv.list.py-list
    val sin-list = ts.py-list
    kkscatter(x-list, t-list, Plus)
    kkplot(x-list, y-list, Red)
    kkplot(x-list, sin-list, Green)
    kkshow()

    // show error
    val x =list(0, iters - 1, fn(x:int) float64(x) ).py-list
    val y = errs.reverse.py-list
    kkplot(x, y)
    kkshow()


