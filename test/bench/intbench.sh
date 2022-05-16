all_variants="int32 ovf tagovf sofa xsofa reno"
all_compilers="clang gcc"
all_benches="nqueens hamming pyth tak"

variants="int32"
compilers="clang"
benches=""

intopts=""
ccopts=""
benchdir="test/bench/koka/"
verbose="no"

do_build="no"
do_run="no"
max_runs=1

function info {
  echo $1
}

function warning {
  echo ""
  echo "warning: $1"
}

while : ; do
  # set flag and flag_arg
  flag="$1"
  case "$flag" in
    *=*)  flag_arg="${flag#*=}"
          flag="${flag%=*}";;
    no-*) flag_arg="0"
          flag="${flag#no-}";;
    none) flag_arg="0" ;;
    *)    flag_arg="1" ;;
  esac
  case "$flag_arg" in
    yes|on|true)  flag_arg="1";;
    no|off|false) flag_arg="0";;
  esac
  case "$flag" in
    "") break;;

    allb) benches="$all_benches";;
    allc) compilers="$all_compilers";;
    allv) variants="$all_variants";;
    
    nqueens) benches="$benches nqueens";;
    hamming) benches="$benches hamming";;
    pyth) benches="$benches pyth";;
    tak)  benches="$benches tak";;

    ovf) variants="$variants ovf";;
    tagovf) variants="$variants tagovf";;
    sofa) variants="$variants sofa";;
    xsofa) variants="$variants xsofa";;
    reno) variants="$variants reno";;

    gcc) compilers="$compilers gcc";;
    gcc-11) compilers="$compilers gcc-11";;

    build) do_build="yes";;
    run)   do_run="yes";;

    asm) ccopts="--ccopts=-save-temps";;

    -n)
        max_runs=$flag_arg;;

    -v|--verbose)
        verbose="yes";;
    -h|--help|-\?|help|\?)
        echo "./intbench.sh [options]"
        echo ""
        echo "options:"
        echo "  -h, --help                   show this help"  
        echo "  -v, --verbose                be verbose (=$verbose)"
        echo ""
        exit 0;;
    *) warning "unknown option \"$1\"." 1>&2
  esac
  shift
done


function set_intopts { # <variant> 
  case "$1" in
    ovf) intopts="--ccopts=-DKK_INT_ARITHMETIC=1";;
    tagovf) intopts="--ccopts=-DKK_INT_ARITHMETIC=2";;
    sofa) intopts="--ccopts=-DKK_INT_ARITHMETIC=3";;
    reno) intopts="--ccopts=-DKK_INT_ARITHMETIC=4";;
    xsofa) intopts="--ccopts=-DKK_INT_ARITHMETIC=3 --ccopts=-DKK_INT_TAG=0";;
    *) intopts="";;
  esac;
}

function build { # <bench> <variant> <cc>
  local options="-O2 --cc=$3 --buildtag=$2 $ccopts"
  if [ "$2" = "int32" ]; then
    options="$options -c $benchdir$1.kk"
  else
    set_intopts "$2"
    options="$options --ccopts=-DKK_INT_NOREFCOUNT $intopts -c $benchdir$1-int.kk"
  fi
  info ""
  info "build: $1, variant: $2, cc: $3, ($options)"
  stack exec koka -- $options
}

function build_all {
  for ccomp in $compilers; do
    for bench in $benches; do
      for variant in $variants; do
        build $bench $variant $ccomp
      done
    done
  done    
}


function run {  #bench variant cc runidx log
  local bench=""
  if [ "$2" = "int32" ]; then
    bench="$1"
  else
    bench="$1_dash_int"
  fi
  cmd=".koka/v2.4.1-$2/$3-drelease/test_bench_koka_$bench"
  info ""
  info "run $4, $1-$3-$2, cmd: $cmd"
  local logrun=".koka/intbench/run.txt"
  $cmd --kktime 2> $logrun
  cat $logrun
  # extract elapsed time
  local line=`head -1 $logrun`
  line=${line#info: elapsed: }
  local elapsed=${line/s,*/}
  echo "$elapsed" >> "$5"
}

function run_all {
  for ccomp in $compilers; do
    for bench in $benches; do
      for variant in $variants; do
        local log=".koka/intbench/$bench-$ccomp-$variant.txt"
        rm -f $log 2> /dev/null          
        for ((runs=1; runs<=$max_runs; runs++)); do
          run $bench $variant $ccomp $runs $log
        done
      done
    done
  done   
}

basetime=""

function avg { #bench variant cc logall
  local log=".koka/intbench/$1-$3-$2.txt"
  local median=`sort -n $log | awk ' { a[i++]=$1; } END { x=int((i+1)/2); if (x < (i+1)/2) print (a[x-1]+a[x])/2; else print a[x-1]; }'`
  local stddev=`awk ' { sum += $1; sumsq += ($1)^2; } END { print sqrt((sumsq - sum^2/NR)/NR); }' < $log`
  if [ "$basetime" = "" ]; then
    basetime="$median"
  fi
  local rmedian=`echo "scale=3; $median / $basetime" | bc`
  local rstddev=`echo "scale=3; $rmedian * $stddev" | bc`
  echo "$1-$3-$2 ${median}s ${rmedian}x ${rstddev}" >> $4
}

function avg_all {
  for bench in $benches; do
    local logall=".koka/intbench/$bench.txt"
    basetime=""
    echo "" > $logall
    for ccomp in $compilers; do    
      for variant in $variants; do
        avg $bench $variant $ccomp $logall
      done
    done
    echo ""
    echo "# benchmark        elapsed  relat.  stddev"
    column -t $logall
  done
}


if [ "$do_build" = "yes" ]; then
  build_all
fi

if [ "$do_run" = "yes" ]; then
  run_all
  avg_all
fi  