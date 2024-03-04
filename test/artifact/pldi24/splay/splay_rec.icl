module splay_rec

import StdEnv,CommandLine

:: Tree = Node !Tree !Int !Tree | Leaf

sumAcc :: !Tree !Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node l x r) acc = sumAcc r (sumAcc l (acc + x))

tsum :: !Tree -> Int
tsum t = sumAcc t 0

minHeight :: !Tree -> Int
minHeight (Node l _ r) = 1 + min (minHeight l) (minHeight r)
minHeight Leaf         = 0

maxHeight :: !Tree -> Int
maxHeight (Node l _ r) = 1 + max (maxHeight l) (maxHeight r)
maxHeight Leaf           = 0

top :: !Tree -> Int
top (Node _ x _) = x
top Leaf         = 0

rndCreate :: !Int !Int -> Int
rndCreate a0 a1 = code {
	ccall rnd_create_c "II:I"
}

rndNext :: !Int -> Int
rndNext a0 = code {
	ccall rnd_next_c "I:I"
}

reportResult :: !Int !Int !Int !Int -> Int
reportResult a0 a1 a2 a3 = code {
	ccall report_result_c "IIII:I"
}

reportInt :: !Int !Int -> Int
reportInt a0 a1 = code {
	ccall report_int_c "II:I"
}

readNum :: ![Char] !Int !Int -> Int
readNum [] acc fail = acc
readNum [c:cs] acc fail =
  let n = digitToInt c in
  if (n < 0 || n > 9)
    (fail)
    (readNum cs (acc * 10 + n) fail)

StringToCharList`	:: !String !Int !Int -> [Char]
StringToCharList` string 0 index
		= 	[]
StringToCharList` string length index
		= [string.[index] : StringToCharList` string (dec length) (inc index)]

stringToCharList	:: !String ->	[Char]
stringToCharList string = StringToCharList` string (size string) 0

readNumber :: ![String] !Int -> Int
readNumber [_:xs:_] fail = readNum (stringToCharList xs) 0 fail
readNumber _ fail = fail

splay_insert :: !Int !*Tree -> (!*Tree,!Int,!*Tree)
splay_insert new_v m=:(Node l v r)
    | new_v==v
        = (l,v,r)
    | new_v<v
        = splay_insert_l new_v m
        = splay_insert_r new_v m
where
    splay_insert_l :: !Int !*Tree -> (!*Tree,!Int,!*Tree)
    splay_insert_l new_v (Node (Node ll lv lr) v r)
        | new_v==lv
            = (ll,lv,Node lr v r)
        | new_v<lv
            # (r_l,r_v,r_r) = splay_insert new_v ll
            = (r_l,r_v,Node r_r lv (Node lr v r))
            # (r_l,r_v,r_r) = splay_insert new_v lr
            = (Node ll lv r_l,r_v,Node r_r v r)
    splay_insert_l new_v (Node Leaf v r)
        = (Leaf,new_v,Node Leaf v r)

    splay_insert_r :: !Int !*Tree -> (!*Tree,!Int,!*Tree)
    splay_insert_r new_v (Node l v (Node rl rv rr))
        | new_v==rv
            = (Node l v rl,rv,rr)
        | new_v>rv
            # (r_l,r_v,r_r) = splay_insert new_v rr
            = (Node (Node l v rl) rv r_l,r_v,r_r)
            # (r_l,r_v,r_r) = splay_insert new_v rl
            = (Node l v r_l,r_v,Node r_r rv rr)
    splay_insert_r new_v (Node l v Leaf)
        = (Node l v Leaf,new_v,Leaf)
splay_insert new_v Leaf
    = (Leaf,new_v,Leaf)

insert :: !*Tree !Int -> *Tree
insert t new_v
  # (l, v, r) = splay_insert new_v t
  = Node l v r

test :: !Int !Int -> Tree
test n iter
  = iloop 0 Leaf
  where 
    iloop :: !Int !*Tree -> *Tree
    iloop i t
      = if (i >= iter) 
         t
         (iloop (i + 1) (nloop 0 t))

    nloop :: !Int !*Tree -> *Tree
    nloop i t
      = if (i >= n) 
          t
          (nloop (i+1) (insert t (rndNext n)))

benchMain :: *World -> *World
benchMain w = 
       let (args, w2) = getCommandLine w in
       let n = readNumber args 100000 in
       let sfc = rndCreate 42 43 in
       if (sfc <> 0) w2 (
         let t = test n 100 in
         let x = reportResult (tsum t) (maxHeight t) (minHeight t) (top t) in
         if (x == 0) w2 w2)

Start :: *World -> *World
Start w = benchMain w
