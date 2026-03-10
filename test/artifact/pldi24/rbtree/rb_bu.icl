module rb_bu

import StdEnv,CommandLine

:: Color = Red | Black
:: Tree = Node !Color !Tree !Int !Int !Tree | Leaf

sumAcc :: !Tree !Int -> Int
sumAcc Leaf acc  = acc
sumAcc (Node _ l x _ r) acc = sumAcc r (sumAcc l (acc + x))

tsum :: !Tree -> Int
tsum t = sumAcc t 0

minHeight :: !Tree -> Int
minHeight (Node _ l _ _ r) = 1 + min (minHeight l) (minHeight r)
minHeight Leaf         = 0

maxHeight :: !Tree -> Int
maxHeight (Node _ l _ _ r) = 1 + max (maxHeight l) (maxHeight r)
maxHeight Leaf           = 0

top :: !Tree -> Int
top (Node _ _ x _ _) = x
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

isRed :: !Tree -> Bool
isRed (Node Red _ _ _ _) = True
isRed _ = False

setBlack :: !Tree -> Tree
setBlack (Node _ l x v r) = Node Black l x v r
setBlack Leaf         = Leaf

:: *Context = Root | L !Color !Context !Int !Int !*Tree | R !Color !*Tree !Int !Int !Context

downto :: !Int !Int !*Tree !Context -> *Tree
downto new_e new_v (Node c l e v r) z
    | new_e==e
        = rebuild (Node c l new_e new_v r) z
    | new_e<e
        = downto new_e new_v l (L c z e v r)
        = downto new_e new_v r (R c l e v z)
downto new_e new_v Leaf z
    = balance Leaf new_e new_v Leaf z

rebuild :: !*Tree !Context -> *Tree
rebuild t (L c z e v r) = rebuild (Node c t e v r) z
rebuild t (R c l e v z) = rebuild (Node c l e v t) z
rebuild t Root = t

balance :: !*Tree !Int !Int !*Tree !Context -> *Tree
balance l k v r (R Black l1 k1 v1 z1)
  = rebuild (Node Black l1 k1 v1 (Node Red l k v r)) z1
balance l k v r (L Black z1 k1 v1 r1)
  = rebuild (Node Black (Node Red l k v r) k1 v1 r1) z1
balance l k v r (R Red l1 k1 v1 (R _ l2 k2 v2 z2))
  = balance (Node Black l2 k2 v2 l1) k1 v1 (Node Black l k v r) z2
balance l k v r (R Red l1 k1 v1 (L _ z2 k2 v2 r2))
  = balance (Node Black l1 k1 v1 l) k v (Node Black r k2 v2 r2) z2
balance l k v r (R Red l1 k1 v1 Root)
  = Node Black l1 k1 v1 (Node Red l k v r)
balance l k v r (L Red (R _ l2 k2 v2 z2) k1 v1 r1)
  = balance (Node Black l2 k2 v2 l) k v (Node Black r k1 v1 r1) z2
balance l k v r (L Red (L _ z2 k2 v2 r2) k1 v1 r1)
  = balance (Node Black l k v r) k1 v1 (Node Black r1 k2 v2 r2) z2
balance l k v r (L Red Root k1 v1 r1)
  = Node Black (Node Red l k v r) k1 v1 r1
balance l k v r Root
  = Node Black l k v r

insert :: !*Tree !Int -> *Tree
insert t new_e
    = downto new_e new_e t Root


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
