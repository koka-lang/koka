type zip
  T(rank : int32, l : zip, k : int32, r : zip)
  E

fun balance-left(z : zip)
  match z
    T(rk1, T(rk2, l2, k2, r2), k1, r1) | rk2 >= rk1 ->
      T(rk2, l2, k2, T(rk1, r2, k1, r1))
    _ -> z

fun balance-right(z : zip)
  match z
    T(rk1, l1, k1, T(rk2, l2, k2, r2)) | rk2 > rk1 ->
      T(rk2, T(rk1, l1, k1, l2), k2, r2)
    _ -> z

fun insert(key : int32, root : zip)
  match root
    E -> T(random-rank(), E, key, E)
    T(rk, l, k, r) ->
      if key < k
      then balance-left(T(rk, insert(key, l), k, r))
      else balance-right(T(rk, l, k, insert(key, r)))

fun insert0(key : int32, rank : int32, root : zip)
  match root
    E -> T(rank, E, key, E)
    T(rk, l, k, r) ->
      if(rank < rk || (rank == rk && key > k)) then
        if key < k
        then T(rk, insert0(key, rank, l), k, r)
        else T(rk, l, k, insert0(key, rank, r))
      else
        if key < k
        then balance-left(T(rk, insert(key, l), k, r))
        else balance-right(T(rk, l, k, insert(key, r)))

fun unzip(key : int32, z : zip)
  match z
    T(rk, l, k, r) ->
      if key < k
      then val (smaller, bigger) = unzip(key, l)
           (smaller, T(rk, bigger, k, r))
      else val (smaller, bigger) = unzip(key, r)
           (T(rk, r, k smaller), bigger)
    E -> (E, E)

fun insert(key : int32, rank : int32, root : zip)
  match root
    E -> T(rank, E, key, E)
    T(rk, l, k, r) ->
      if(rank < rk || (rank == rk && key > k)) then
        if key < k
        then T(rk, insert(key, rank, l), k, r)
        else T(rk, l, k, insert(key, rank, r))
      else val (smaller, bigger) = unzip(key, root)
           T(rank, smaller, key, bigger)