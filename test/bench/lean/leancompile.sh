function run {
  echo $1
  $1
}

run "lean --c=$1.c  $1"
run "leanc -O3 -DNDEBUG -o lean-${1%.lean} $1.c"
