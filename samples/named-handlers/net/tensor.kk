module tensor

import std/num/float64
import std/num/random


//-------------------------------------------------------------------------------------------------
// helper fuctions of dtype
//-------------------------------------------------------------------------------------------------


// println the shape of tensor
pub fun show(dtype: (int, int)): <> string {
    val (i, j) = dtype
    "(" ++ show(i) ++ ", " ++ show(j) ++ ")"
}

pub fun println(dtype: (int, int)): console () {
    val (i, j) = dtype
    val str = "(" ++ show(i) ++ ", " ++ show(j) ++ ")"
    println(str)
}

pub fun (==)(dtype1: (int, int), dtype2: (int, int)): <> bool {
    val (x, y) = dtype1
    val (i, j) = dtype2
    (x == i) && (y == j)
}

//-------------------------------------------------------------------------------------------------
// struct of tensor
//-------------------------------------------------------------------------------------------------

pub struct tensor {
    shape: (int, int)
    data: vector<float64>
}


pub fun print(xs: list<float64>): console () {
    print("[")
    foreach-indexed(xs, fn(i, x) {
        print(x)
        if (i == length(xs) - 1) then print("")
        else print(", ")
    })
    print("]")
}

// print data of tensor
pub fun print(ts: tensor): console () {
    print(ts.data.list)
}

// println data of tensor
pub fun println(ts: tensor): console () {
    val (i, j) = ts.shape
    val s = "dtype: (" ++ show(i) ++ ", " ++ show(j) ++ ") "
    print(s)
    print(ts);print("\n")
}

pub fun list(ts: tensor): <> list<float64> {
    ts.data.list
}


//-------------------------------------------------------------------------------------------------
// unsafe functions of vectors
//-------------------------------------------------------------------------------------------------

// unsafe code
// return the element at position `index`  in vector `v`  without bounds check!
pub inline extern unsafe-idx: forall<a> (v: vector<a>, i: ssize_t) -> <> a {
  c inline "kk_vector_at_borrow(#1,#2)"
}

// unsafe code
// assgin the x to vector without bound check
pub inline extern unsafe-assign: forall<a> (v: vector<a>, i: ssize_t, x : a) -> <> () {
  c "kk_vector_unsafe_assign"
}

// unsafe code
// create new vector withou initial state
pub inline extern unsafe-vector: forall<a> (n: ssize_t) -> total vector<a> {
  c inline "kk_vector_alloc_uninit(#1,NULL,kk_context())"
}

// unsafe code
// 
fun foreach-indexedt(v: vector<a>, f: (a,ssize_t) -> e ()): e () {
  for(0, v.length.dec) fn(i) {
    f(v.unsafe-idx(i.ssize_t), i.ssize_t)
  }
}



//-------------------------------------------------------------------------------------------------
// Constructors of tensors
//-------------------------------------------------------------------------------------------------

// create a new tensor of (i, j) with default
pub fun tensor(dtype: (int, int), default: float64): <> tensor {
    val (i, j) = dtype
    val v = vector(i * j, default)
    Tensor(dtype, v)
}

// create a new tensor from a list
pub fun tensor(xs: list<list<float64>>): <> tensor {
    val i = xs.length
    val j = xs.head([]).length
    val data = xs.concat.vector
    Tensor((i, j), data)
}

pub inline fun tensor(xs: list<float64>): <> tensor {
    tensor([xs])
}

pub inline fun tensor(d: float64): <> tensor {
    tensor([[d]])
}

// create a new tensor of (i, j) with 0
pub inline fun zeros(dtype: (int, int)): <> tensor {
    tensor(dtype, 0.0)
}

// create a new tensor of (i, j) with 1
pub inline fun ones(dtype: (int, int)): <> tensor {
    tensor(dtype, 1.0)
}

// create  a new tensor with 0
pub inline fun zeros_like(ts: tensor): <> tensor {
    tensor(ts.shape, 0.0)
}

// create  a new tensor with 0
pub inline fun ones_like(ts: tensor): <> tensor {
   tensor(ts.shape, 1.0)
}

pub fun randn(dtype: (int, int)): random tensor {
    val (x, y) = dtype
    val v = unsafe-vector((x * y).ssize_t)
    for(0, x * y - 1) fn(i) {
        val m = random-float64()
        val n = random-float64()
        val r = sqrt(0.0 - 2.0 * ln(m)) * sin(2.0 * pi * n)
        unsafe-assign(v, i.ssize_t, r)
    }
    Tensor(dtype, v)
}


//-------------------------------------------------------------------------------------------------
// helper functions of tensors
//-------------------------------------------------------------------------------------------------

// return the element at position `index`  in vector `v`  withbounds check
pub fun at(ts: tensor, i: int, j: int): <> float64 {
    val (x, y) = ts.shape
    assert("index out of bounds in at", i < x && j < y)
    val idx = i * y + j
    ts.data.unsafe-idx(idx.ssize_t)
}

// assign the 
pub fun assign(ts: tensor, i: int, j: int, z: float64): <> () {
    val (x, y) = ts.shape
    assert("index out of bounds in assign", i < x && j < y)
    val idx = i * y + j
    ts.data.unsafe-assign(idx.ssize_t, z)
}


// unsafe code
// apply a total function `f` to each element in a vector `v`
fun vmap(v : vector<float64>, w: vector<float64>, f: (float64, float64) -> <> float64 ): <> vector<float64> {
  val u = unsafe-vector(v.length.ssize_t)
  v.foreach-indexedt fn(x, i) {
    val y = w.unsafe-idx(i)
    unsafe-assign(u, i, f(x, y))
  }
  u
}


pub fun map(t: tensor, f: float64 -> e float64): e tensor {
    val v = t.data.map(f)
    Tensor(t.shape, v)
}

//-------------------------------------------------------------------------------------------------
// binary operations between tensor and tensor
//-------------------------------------------------------------------------------------------------

// unsafe code
// add ts1 and ts2 to each element
pub fun (+)(ts1: tensor, ts2: tensor): <> tensor {
    assert("add: not match shape, " ++ show(ts1.shape) ++ " != " ++ show(ts2.shape), ts1.shape == ts2.shape)
    val dtype = ts1.shape
    val data = vmap(ts1.data, ts2.data, (+))
    Tensor(dtype, data)
}

// unsafe code
// sub ts1 and ts2 to each element
pub fun (-)(ts1: tensor, ts2: tensor): <> tensor {
    assert("sub: not match shape", ts1.shape == ts2.shape)
    val dtype = ts1.shape
    val data = vmap(ts1.data, ts2.data, (-))
    Tensor(dtype, data)
}

// unsafe code
// mul ts1 and ts2 to each element
pub fun (*)(ts1: tensor, ts2: tensor): <> tensor {
    assert("mul: not match shape", ts1.shape == ts2.shape)
    val dtype = ts1.shape
    val data = vmap(ts1.data, ts2.data, (*))
    Tensor(dtype, data)
}

// unsafe code
// div ts1 and ts2 to each element
pub fun (/)(ts1: tensor, ts2: tensor): <> tensor {
    assert("div: not match shape", ts1.shape == ts2.shape)
    val dtype = ts1.shape
    val data = vmap(ts1.data, ts2.data, (/))
    Tensor(dtype, data)
}


//-------------------------------------------------------------------------------------------------
// binary operations between float64 and tensor
//-------------------------------------------------------------------------------------------------

pub inline fun (+)(a: float64, ts: tensor): <> tensor {
    ts.map(fn(x) { a + x })
} 

pub inline fun (+)(ts: tensor, a: float64): <> tensor {
    ts.map(fn(x) { x + a })
} 

pub inline fun (-)(a: float64, ts: tensor): <> tensor {
    ts.map(fn(x) { a - x })
} 

pub inline fun (-)(ts: tensor, a: float64): <> tensor {
    ts.map(fn(x) { x - a })
} 

pub inline fun (*)(a: float64, ts: tensor): <> tensor {
    ts.map(fn(x) { a * x })
} 

pub inline fun (*)(ts: tensor, a: float64): <> tensor {
    ts.map(fn(x) { x * a })
} 

pub inline fun (/)(a: float64, ts: tensor): <> tensor {
    ts.map(fn(x) { a / x })
} 

pub inline fun (/)(ts: tensor, a: float64): <> tensor {
    ts.map(fn(x) { x / x })
}

pub inline fun expt(ts: tensor): <> tensor {
    ts.map(fn(x) { exp(x) })
}

pub inline fun logt(ts: tensor): <> tensor {
    ts.map(fn(x) { ln(x) })
}

// sin
pub fun sint(ts: tensor): <> tensor {
    ts.map(fn(x) { sin(x) })
}

// multiplication of tensor
// 配列の範囲外アクセスをしてしまい，実行時エラーになる場合がある
// Kokaコンパイラのバグっぽい
// inline 化するとバグる
pub fun matmult(ts1: tensor, ts2: tensor): <> tensor {
    val (x, y) = ts1.shape
    val (z, w) = ts2.shape
    assert("not match matmul shape, " ++ show(ts1.shape) ++ " != " ++ show(ts2.shape) , y == z)
    
    val ts = tensor((x, w), 0.0)
    // what is e?
    ts.data.foreach-indexed fn(idx, e) {
         val (i, j) = (idx / w, idx % w)
         var s := e
         for(0, y - 1) fn(k) {
             val e1 = ts1.at(i, k)
             val e2 = ts2.at(k, j)
             s := s + e1 * e2
         }
         ts.assign(i, j, s)
    }
    ts
}

// transpose of tensor
// TODO: we may do not have use for loop?
// TODO: (i, j) -> (j, i)
pub fun transposet(ts1: tensor): <> tensor {
    val (i, j) = ts1.shape
    val ts = tensor((j, i), 0.0) 
    for(0, i - 1) fn(x) {
        for(0, j - 1) fn(y) {
            val e = ts1.at(x, y)
            ts.assign(y, x, e)
        }
    }
    ts
}

// reshape tensor
pub fun reshapet(ts: tensor, dtype: (int, int)): <> tensor {
    val (i, j) = ts.shape
    val (x, y) = dtype
    assert("not reshpe tensor", i * j == x * y)
    Tensor(dtype, ts.data)
}

// sum of all element in tensor
// TODO: according to axis
// TODO: shoulf use sum_tot
pub fun sumt(ts: tensor): <> tensor {
    var tot := 0.0
    ts.data.foreach fn(x) {
        tot := tot + x
    }

    tensor(tot)
}

// broadcast to shape from tensor
pub fun broadcast_tot(ts: tensor, todtype: (int, int)): <> tensor {
    val (tox, toy) = todtype 
    val (x, y) = ts.shape
    if (y == 1 && x == 1) then {
        val e = ts.at(0, 0)
        tensor(todtype, e)
    } elif (y == 1 && x == tox) then {
        val rs = tensor((x, toy), 0.0)
        for(0, x - 1) fn(i) {
            val e = ts.at(i, 0)
            for(0, toy - 1) fn(j) {
                rs.assign(i, j, e)
            }
        }
        rs
    } elif (y == toy && x == 1) then  {
        val rs = tensor((tox, y), 0.0)
        for(0, y - 1) fn(j) {
            val e = ts.at(0, j)
            for(0, tox - 1) fn(i) {
                rs.assign(i, j, e)
            }
        }
        rs
    } elif (y == toy && x == tox) then {
        ts // deep copy??
    } else {
        // TODO: should remove this branch??
        assert("can not braodcast", False)
        tensor([[]]) // dummy tensor
    }
}

// sum to shape 
// adhoc implementation
pub fun sum_tot(ts: tensor, todtype: (int, int)): <> tensor {
    val (tox, toy) = todtype
    val (x, y) = ts.shape
    if (tox == 1 && toy == 1) then {
        val rs = tensor((tox, toy), 0.0)
        var tot := 0.0
        for(0, x - 1) fn(i) {
            for(0, y - 1) fn(j) {
                val e = ts.at(i, j)
                tot := tot + e
            }
        }
        rs.assign(0, 0, tot)
        rs
    } elif (tox == x && toy == 1) then {
        val rs = tensor((tox, toy), 0.0)
        for(0, x - 1) fn(i) {
            var tot := 0.0
            for(0, y - 1) fn(j) {
                val e = ts.at(i, j)
                tot := tot + e
            }
            rs.assign(i, 0, tot)
        }
        rs
    } elif (tox == 1 && toy == y) then {
        val rs = tensor((tox, toy), 0.0)
        for(0, y - 1) fn(j) {
            var tot := 0.0
            for(0, x - 1) fn(i) {
                val e = ts.at(i, j)
                tot := tot + e
            }
            rs.assign(0, j, tot)
        }
        rs
    } elif (tox == x && toy == y) then {
        ts
    } else {
        assert("can not sum_to", False)
        tensor([[]])
    }
}

// linear 
// w * x + b
pub fun lineart(x: tensor, w: tensor, b: tensor): <> tensor {
    val (x1, y1) = x.shape
    val (x2, y2) = w.shape
    val (x3, y3) = b.shape
    assert("not match matmul shape, " ++ show(x.shape) ++ " != " ++ show(w.shape) , y1 == x2)
    assert("not match bias shape, " ++ show(w.shape) ++ "!=" ++ show(b.shape), x3 == 1 && y2 == y3)
   
    val ts = tensor((x1, y2), 0.0)

    // what is e?
    ts.data.foreach-indexed fn(idx, e) {
         val (i, j) = (idx / y2, idx % y2)
         val bias = b.at(0, j)
         var s := e
         for(0, y1 - 1) fn(k) {
             val e1 = x.at(i, k)
             val e2 = w.at(k, j)
             s := s + e1 * e2
         }
         s := s + bias
         ts.assign(i, j, s)
    }
    ts 
}

