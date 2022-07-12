module variable

import tensor

pub struct variable<h> {
    data: tensor
    grad: ref<h, tensor>
}

pub alias vv<h> = variable<h>

// fun show(v : variable<h, tensor>) : <read<h>> string {
//     "Var(value : " + show(v.value) + ", grad : " + show(!v.grad) + ")"
// }

pub fun println(v: variable<h>): <read<h>, console> () {
    print(show(v.shape) ++ ": ")
    print("Var(data: ")
    print(v.data)
    print(", grad: ")
    print(!v.grad)
    println(" )")
}

pub fun list(v: variable<h>): <> list<float64> {
    v.data.list
}

//-------------------------------------------------------------------------------------------------
// construtors of variable
//-------------------------------------------------------------------------------------------------

pub fun variable(xs: list<list<float64>>): alloc<h> variable<h> {
    val ts = tensor(xs)
    val gt = zeros_like(ts)
    Variable(ts, ref(gt))
}

pub fun variable(xs: list<float64>): alloc<h> variable<h> {
    variable([xs])
}

pub fun variable(ts: tensor): alloc<h> variable<h> {
    val gt = zeros_like(ts)
    Variable(ts, ref(gt))
}

pub fun variable(d: float64): alloc<h> variable<h> {
    variable([[d]])
}

pub fun variable(dtype: (int, int), default: float64): alloc<h> variable<h> {
    val ts = tensor(dtype, default)
    val gt = zeros_like(ts)
    Variable(ts, ref(gt))
}

//-------------------------------------------------------------------------------------------------
// helper functions of variable
//-------------------------------------------------------------------------------------------------


pub inline fun addv(x: variable<h>, y: variable<h>): alloc<h> variable<h> {
    Variable(x.data + y.data, ref(zeros_like(x.data)))
}

pub inline fun subv(x: variable<h>, y: variable<h>): alloc<h> variable<h> {
    Variable(x.data - y.data, ref(zeros_like(x.data)))
}

pub inline fun mulv(x: variable<h>, y: variable<h>): alloc<h> variable<h> {
    Variable(x.data * y.data, ref(zeros_like(x.data)))
}

pub inline fun divv(x: variable<h>, y: variable<h>): alloc<h> variable<h> {
    Variable(x.data / y.data, ref(zeros_like(x.data)))
}

pub inline fun expv(x: variable<h>): alloc<h> variable<h> {
    val ts = expt(x.data) 
    Variable(ts, ref(zeros_like(x.data)))
}

pub inline fun logv(x: variable<h>): alloc<h> variable<h> {
    val ts = logt(x.data)
    Variable(ts, ref(zeros_like(x.data)))
}

pub inline fun matmulv(x: variable<h>, y:variable<h>): alloc<h> variable<h> {
    val ts = matmult(x.data, y.data)
    Variable(ts, ref(zeros_like(ts)))
}

pub inline fun transposev(x: variable<h>): alloc<h> variable<h> {
    Variable(x.data.transposet, ref(zeros_like(x.data)))
}

pub inline fun sumv(x: variable<h>): alloc<h> variable<h> {
    Variable(x.data.sumt, ref(zeros((1, 1))))
}

pub inline fun broadcast_tov(x: variable<h>, dtype: (int, int)): alloc<h> variable<h> {
    Variable(x.data.broadcast_tot(dtype), ref(zeros(dtype)))
}

pub inline fun sum_tov(x: variable<h>, dtype: (int, int)): alloc<h> variable<h> {
    Variable(x.data.sum_tot(dtype), ref(zeros(dtype)))
}

pub inline fun shape(x: variable<h>): (int, int) {
    x.data.shape
}

// ブロードキャストしないといけないがしていない．のでエラー
pub inline fun linearv(x: variable<h>, w: variable<h>, b: variable<h>): alloc<h> variable<h> {
    val (xr, _) = x.shape
    val (_, wc) = w.shape
    Variable(lineart(x.data, w.data, b.data), ref(zeros((xr, wc))))
}
