module layers

import tensor
import variable
// import autodiff
import std/num/float64
import std/num/random

//-------------------------------------------------------------------------------------------------
// kayers
//-------------------------------------------------------------------------------------------------
pub effect layer<a> {
    ctl new-linear(i: int, o: int): linear<a>
}

pub fun layers(action) {
    with handler {
        ctl new-linear(i, o) {
            handle-linear(i, o, resume)
        }
    }
    action()
}


//-------------------------------------------------------------------------------------------------
// linear layer 
//-------------------------------------------------------------------------------------------------

pub named effect linear<a> {
    fun get-weight(): a
    fun set-weight(w: a): ()
    fun get-bias(): a
    fun set-bias(b: a): ()
    fun optimizer-step(lr: float64): ()
}

pub fun handle-linear(i: int, o: int, action) {
    var w := variable(1.0 / sqrt(o.float64) * randn((i, o)))
    var b := variable(0.01 * randn((1, o)))
    with h = named handler {
        fun get-weight()   { w }
        fun get-bias()     { b }
        fun set-weight(ws) { w := ws }
        fun set-bias(bs)   { b := bs }
        fun optimizer-step(lr) {
            val t1 = w.data - lr * !w.grad
            val t2 = b.data - lr * !b.grad
            w := variable(t1)
            b := variable(t2)
        }
    }
    action(h)
}

pub fun linear(i : int, o : int, action) 
  handle-linear(i,o,action)


// fun linear-handler(): <layer<variable<h>> variable<h> {

// }

//-------------------------------------------------------------------------------------------------
// conv1d layer 
//-------------------------------------------------------------------------------------------------

//-------------------------------------------------------------------------------------------------
// conv2d layer 
//-------------------------------------------------------------------------------------------------

//-------------------------------------------------------------------------------------------------
// dropout layer 
//-------------------------------------------------------------------------------------------------


