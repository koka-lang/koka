module zip_bu

import StdEnv,CommandLine

:: Tree = Node !Int !Tree !Int !Tree | Leaf

sumAcc :: !Tree !Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node _ l x r) acc = sumAcc r (sumAcc l (acc + x))

tsum :: !Tree -> Int
tsum t = sumAcc t 0

minHeight :: !Tree -> Int
minHeight (Node _ l _ r) = 1 + min (minHeight l) (minHeight r)
minHeight Leaf         = 0

maxHeight :: !Tree -> Int
maxHeight (Node _ l _ r) = 1 + max (maxHeight l) (maxHeight r)
maxHeight Leaf           = 0

top :: !Tree -> Int
top (Node _ _ x _) = x
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

:: *Context = Root | L !Int !Context !Int !*Tree | R !Int !*Tree !Int !Context

rankOf :: !Int -> Int
rankOf a0 = code {
	ccall rank_of_c "I:I"
}

isHigherRank :: !Int !Int !Int !Int -> Bool
isHigherRank r1 k1 r2 k2 =
  r1 > r2 || (r1 == r2 && k1 < k2)

hasKey :: !Int !*Tree -> Bool
hasKey k (Node _ l x r) = x == k
hasKey k Leaf = False

downto :: !Int !Int !*Tree -> *Tree
downto new_e rank t=:(Node rk l e r)
    | isHigherRank rk e rank new_e
        = go1 new_e rank t
        = go2 new_e rank t
    where
    go1 new_e rank (Node rk l e r)
        | e<new_e
            = Node rk l e (downto new_e rank r)
            = Node rk (downto new_e rank l) e r
    go2 new_e rank t=:(Node rk l e r)
        | new_e == e
            = t
            # (s, b) = unzip t new_e Root Root
            = Node rank s new_e b
downto new_e rank Leaf
    = Node rank Leaf new_e Leaf

unzip :: !*Tree !Int !Context !Context -> (*Tree, *Tree)
unzip (Node rk l e r) new_e zs zb
   | e<new_e
       = unzip r new_e (R rk l e zs) zb
       = unzip l new_e zs (L rk zb e r)
unzip Leaf new_e zs zb = (rebuild Leaf zs, rebuild Leaf zb)

rebuild :: !*Tree !Context -> *Tree
rebuild t (L rk c e r) = rebuild (Node rk t e r) c
rebuild t (R rk l e c) = rebuild (Node rk l e t) c
rebuild t Root = t

insert :: !*Tree !Int -> *Tree
insert t new_e
    = downto new_e (rankOf new_e) t


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
