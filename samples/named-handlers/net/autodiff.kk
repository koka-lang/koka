module autodiff

import tensor
import variable
import layers

//-------------------------------------------------------------------------------------------------
// derivetive effects
//-------------------------------------------------------------------------------------------------


pub effect deriv<a> {
    // basic functions
    fun constE(x: tensor): a
    ctl addE(x: a, y: a): a
    ctl mulE(x: a, y: a): a
    ctl subE(x: a, y: a): a
    ctl divE(x: a, y: a): a
    ctl matmulE(x: a, y: a): a
    ctl sumE(x: a): a
    ctl expE(x: a): a
    ctl logE(x: a): a
    // ctl sinE(x: a): a
    // ctl cosE(x: a): a

    // helper functions
    // ctl reshapeE(x: a): a
    ctl broadcast_toE(x: a, todtype: (int, int)): a
    ctl sum_toE(x: a, todtype: (int, int)): a
    
    // // deep learning
    ctl linearE(layer: linear<a>, x: a): a

    // // activation functions
    // ctl sigmoidE(x: a): a
    // ctl reluE(x: a): a
    // ctl tanhE(x: a): a

}




//-------------------------------------------------------------------------------------------------
// handler for backpropagation 
//-------------------------------------------------------------------------------------------------

//逆伝搬のハンドラ
pub fun backprop(f: () -> <st<h>, deriv<variable<h>>, div, exn, console | e> variable<h>): <st<h>, div, exn, console | e> variable<h> {
    with handler {
        fun constE(x) {
            variable(x)
        }
        ctl addE(x, y) {
            val r = addv(x, y)
            val ret = resume(r)
            set(x.grad, !x.grad + !r.grad)
            set(y.grad, !y.grad + !r.grad)
            ret
        }
        ctl subE(x, y) {
            val r = subv(x, y)
            val ret = resume(r)
            set(x.grad, !x.grad + !r.grad)
            set(y.grad, !y.grad - !r.grad)
            ret 
        }
        ctl mulE(x, y) {
            val r = mulv(x, y)
            val ret = resume(r)
            // TODO: more effeicient code 
            set(x.grad, !x.grad + !r.grad * y.data)
            set(y.grad, !y.grad + !r.grad * x.data)
            ret  
        }
        ctl divE(x, y) {
            val r = divv(x, y)
            val ret = resume(r)
            // TODO: more effeicient code)
            set(x.grad, !x.grad + !r.grad / y.data)
            set(y.grad, !y.grad - !r.grad * (x.data / (y.data * y.data)))
            ret
        }
        ctl expE(x) {
            val r = expv(x)
            val ret = resume(r)
            set(x.grad, !x.grad + !r.grad * r.data)
            ret
        }
        ctl logE(x) {
            val r = logv(x)
            val ret = resume(r)
            set(x.grad, !x.grad + !r.grad / x.data)
            ret
        }
        ctl matmulE(x, w) {
            val r = matmulv(x, w)
            val ret = resume(r)
            // TODO: more efficient code
            set(x.grad, matmult(!r.grad, w.data.transposet))
            set(w.grad, matmult(x.data.transposet, !r.grad))
            ret
        }
        ctl sumE(x) {
            val r = sumv(x)
            val ret = resume(r)
            // TODO: more efficient code
            // consider broad cast
            // Assume that sum: R^n * m -> R
            val ts = tensor(x.shape, at(!r.grad, 0, 0))
            set(x.grad, !x.grad + ts)
            ret
        }
        ctl broadcast_toE(x, todtype) {
            val r = broadcast_tov(x, todtype)
            val ret = resume(r)
            val dtype = x.shape
            set(x.grad, !x.grad + (!r.grad).sum_tot(dtype))
            ret
        }
        ctl sum_toE(x, todtype) {
           val r = sum_tov(x, todtype)
           val ret  = resume(r)
           val dtype = x.shape
           set(x.grad, !x.grad + (!r.grad).broadcast_tot(dtype))
           ret
        }
        ctl linearE(layer, x) {
            val w = layer.get-weight
            val b = layer.get-bias
            val r = linearv(x, w, b)
            val ret = resume(r)
            set(x.grad, matmult(!r.grad, w.data.transposet))
            set(w.grad, matmult(x.data.transposet, !r.grad))
            set(b.grad, sum_tot(!r.grad, b.shape))
            layer.set-weight(w)
            layer.set-bias(b)
            ret
        }
    }

    val t = f()
    set(t.grad, ones_like(t.data))
    t
}

// 順伝搬のみのハンドラ
pub fun eval(f: () -> <st<h>, deriv<variable<h>>, div, exn | e> a): <st<h>, div, exn | e> a {
    with handler {
        fun constE(x) { variable(x) }
        fun addE(x, y) { addv(x, y) }
        fun subE(x, y) { subv(x, y) }
        fun mulE(x, y) { mulv(x, y) }
        fun divE(x, y) { divv(x, y) }
        fun expE(x) { expv(x) }
        fun logE(x) { logv(x) }
        fun matmulE(x, w) { matmulv(x, w) }
        fun sumE(x) { sumv(x) }
        fun broadcast_toE(x, todtype) { broadcast_tov(x, todtype) }
        fun sum_toE(x, todtype) { sum_tov(x, todtype) }
        fun linearE(layer, x) {             
            val w = layer.get-weight
            val b = layer.get-bias
            linearv(x, w, b)
        }
    }
    f()
}


// //------------------------------------------------------------------------------------------------
// // helper functions of deriv effects
// //------------------------------------------------------------------------------------------------

// BUG: if  branchが増えるとコンパイル時間が増えて、コンパイルできない
// BUG: 体感だとO(2^n)ぐらいで増加してそう
pub fun broadcast_then(x: variable<h>, y: variable<h>, bopE: (variable<h>, variable<h>) -> deriv<variable<h>> variable<h>): deriv<variable<h>> variable<h> {
    val (xr, xc) = x.shape
    val (yr, yc) = y.shape
    
    if (xr == 1 && xc == 1) then {
        val z = broadcast_toE(x, (yr, yc))
        bopE(z, y)
    } elif (xr == 1 && yr != 1 && xc == yc) then {
        val z = broadcast_toE(x, (yr, xc))
        bopE(z, y)
    } elif (xr == yr && xc == 1 && yc != 1) then {
        val z = broadcast_toE(x, (xr, yc))
        bopE(z, y)    
    } elif (yr == 1 && yc == 1) then {
        val z = broadcast_toE(y, (xr, xc))
        bopE(x, z)
    } elif (xr != 1 && yr == 1 && xc == yc) then {
        val z = broadcast_toE(y, (xr, yc))
        bopE(x, z)
    }  elif (xr == yr && xc != 1 && yc == 1) then {
        val z = broadcast_toE(y, (yr, xc))
        bopE(x, z)
    // } elif (xr == 1 && xc != 1 && yr != 1 && yc == 1) then {
    //     val z = broadcast_toE(x, (yr, xc))
    //     val w = broadcast_toE(y, (yr, xc))
    //     bopE(z, w)
    // } elif (xr != 1 && xc == 1 && yr == 1 && yc != 1) then {
    //     val z = broadcast_toE(x, (xr, yc))
    //     val w = broadcast_toE(y, (xr, yc))
    //     bopE(z, w)
    } else {
        bopE(x, y)
    }
}


pub fun (+)(x: variable<h>, y: variable<h>): deriv<variable<h>> variable<h> {
    broadcast_then(x, y, addE)
}

pub fun (+)(x: variable<h>, y: float64): deriv<variable<h>> variable<h> {
    broadcast_then(x, constE(tensor(y)), addE)
}

pub fun (+)(x: float64, y: variable<h>): deriv<variable<h>> variable<h> {
    broadcast_then(constE(tensor(x)), y, addE)
}

pub fun (-)(x: variable<h>, y: variable<h>): deriv<variable<h>> variable<h> {
    broadcast_then(x, y, subE)
}

pub fun (-)(x: variable<h>, y: float64): deriv<variable<h>> variable<h> {
    broadcast_then(x, constE(tensor(y)), subE)
}

pub fun (-)(x: float64, y: variable<h>): deriv<variable<h>> variable<h> {
    broadcast_then(constE(tensor(x)), y, subE)
}

pub fun (*)(x: variable<h>, y: variable<h>): deriv<variable<h>> variable<h> {
   broadcast_then(x, y, mulE)
}

pub fun (*)(x: variable<h>, y: float64): deriv<variable<h>> variable<h> {
   broadcast_then(x, constE(tensor(y)), mulE)
}

pub fun (*)(x: float64, y: variable<h>): deriv<variable<h>> variable<h> {
   broadcast_then(constE(tensor(x)), y, mulE)
}

pub fun (/)(x: variable<h>, y: variable<h>): deriv<variable<h>> variable<h> {
    broadcast_then(x, y, divE)
}

pub fun (/)(x: variable<h>, y: float64): deriv<variable<h>> variable<h> {
    broadcast_then(x, constE(tensor(y)), divE)
}

pub fun (/)(x: float64, y: variable<h>): deriv<variable<h>> variable<h> {
    broadcast_then(constE(tensor(x)), y, divE)
}


pub inline fun exp(x: variable<h>): deriv<variable<h>> variable<h> {
    expE(x)
}

pub inline fun log(x: variable<h>): deriv<variable<h>> variable<h> {
    logE(x)
}

pub inline fun matmul(x: variable<h>, y: variable<h>): deriv<variable<h>> variable<h> {
    matmulE(x, y)
}

pub inline fun sum(x: variable<h>): deriv<variable<h>> variable<h> {
    sumE(x)
}

pub inline fun broadcast_to(x: variable<h>, dtype: (int, int)): deriv<variable<h>> variable<h> {
    broadcast_toE(x, dtype)
}

pub inline fun sum_to(x: variable<h>, dtype: (int, int)): deriv<variable<h>> variable<h> {
    sum_toE(x, dtype)
}