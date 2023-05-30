
# 
runparams="100000" # "1 10 100 1000 10000 100000 1000000"
runparams_small="1 10 100 1000"
benchmarks="rbtree ftree msort qsort tmap"
graphvariants="fip std-reuse std stl stl-mi std-mi"

# note: order matters as it is made relative to the first 
benches_tmapkk="tmap/tmap-fip.kk tmap/tmap-std.kk"
benches_tmapc="tmap/tmap-fip.c tmap/tmap-std.c"
benches_rbtreekk="rbtree/rbtree-fip.kk rbtree/rbtree-fip-icfp.kk rbtree/rbtree-std.kk rbtree/rbtree-fip-clrs.kk"
benches_rbtreec="rbtree/rbtree-clrs.c rbtree/rbtree-clrs-full.c rbtree/rbtree-stl.cpp"
benches_msortkk="msort/msort-fip.kk msort/msort-std.kk"
benches_qsortkk="qsort/qsort-fip.kk qsort/qsort-std.kk"
benches_ftreekk="ftree/ftree-fip.kk ftree/ftree-std.kk"

benches_rbtree="$benches_rbtreekk $benches_rbtreec"
benches_msort="$benches_msortkk"
benches_qsort="$benches_qsortkk"
benches_tmap="$benches_tmapkk $benches_tmapc"
benches_ftree="$benches_ftreekk"
benches_all="$benches_rbtree $benches_ftree $benches_msort $benches_qsort $benches_tmap"

# get this by running `stack path | grep local-install-root`` in the koka development directory 
# koka_install_dir="/mnt/c/Users/daan/dev/koka/.stack-work/install/x86_64-linux-tinfo6/665c0f3ba306de11186f0f92ea0ca8305283b035f4fa2dfb5c2b12a96689073b/8.10.7"
# koka_install_dir="/Users/daan/dev/koka/.stack-work/install/aarch64-osx/b63e887d74237da23db5e39821e14b1f1662540a2b2d9c63219cb143bf61a966/8.10.7"
koka_install_dir="/Users/anton/orga/phd/koka/.stack-work/install/aarch64-osx/1a7c21de82e435443ed6a5394d51d0409374699330e76b45953b1b5661520371/8.10.7"

# if kokainstall does not exist, try to find it from the local koka development directory
koka_dev_dir="../../../koka"

if ! [ -d "$koka_install_dir" ]; then
  if [ -d "$koka_dev_dir" ]; then
    pushd "$koka_dev_dir"
    koka_install_dir=`stack path | grep local-install-root`
    koka_install_dir="${koka_install_dir#local-install-root: }"
    popd
  fi
fi      

koka="${koka_install_dir}/bin/koka"
koka_ver="v2.4.1"

echo "using koka: $koka"

coutdir=".koka/ccomp"
copts="-lpthread"

cppoutdir=".koka/cppcomp"
cppopts="-lpthread"

mimalloc_o="/usr/local/lib/mimalloc-2.1/mimalloc.o"

gtime="/usr/bin/time"
if command -v "gtime"; then
  gtime=`which gtime`
fi

cppcomp="clang++"
ccomp="clang"
benches=""

kkopts=""
benchdir="src"
verbose="no"

do_build="no"
do_run="no"
do_avg="no"
do_graph="no"
max_runs=1

trap ctrl_c INT

function ctrl_c() {
  echo "Ctrl + C happened"
  exit
}

function ensure_dir {
  if ! [ -d "$1" ]; then 
    mkdir -p "$1"
  fi
}

function info {
  echo $1
}

function warning {
  echo ""
  echo "warning: $1"
}

ensure_dir "log"
ensure_dir ".koka/ccomp"
ensure_dir ".koka/cppcomp"

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

    allb) benches="$benches_all";;

    allkk)    benches="$benches $benches_tmapkk $benches_rbtreekk $benches_fingerkk $benches_sortkk";;
    allc)     benches="$benches $benches_tmapc $benches_rbtreec";;
    tmap)     benches="$benches $benches_tmap";;
    rbtree)   benches="$benches $benches_rbtree";;
    qsort)    benches="$benches $benches_qsort";;
    msort)    benches="$benches $benches_msort";;
    sort)     benches="$benches $benches_msort $benches_qsort";;
    ftree)    benches="$benches $benches_ftree";;
    tmap)     benches="$benches $benches_tmapc";;
    

    ccomp) ccomp="$flag_arg";;
    cppcomp) cppcomp="$flag_arg";;
    gcc)   ccomp="gcc"
           cppcomp="g++";;
    clang) ccomp="clang"
           cppcomp="clang++";;

    build) do_build="yes";;
    run)   do_run="yes"
           do_avg="yes";;
    graph) do_graph="yes";;
    avg)   do_avg="yes";;

    asm)    kkopts="$kkopts --core --ccopts=-save-temps";;
    core)   kkopts="$kkopts --core";;
    nounroll) kkopts="--fno-unroll";;

    koka)  koka="$flag_arg";;
    ocamlopt) ocamlopt="$flag_arg";;
    
    small) runparams="$runparams_small";;

    -n|-i)
        max_runs=$flag_arg;;

    -v|--verbose)
        verbose="yes";;
    -h|--help|-\?|help|\?)
        echo "./bench.sh [options]"
        echo ""
        echo "options:"
        echo "  -h, --help                   show this help"  
        echo "  -v, --verbose                be verbose (=$verbose)"
        echo ""
        echo "see 'bench.sh' for all available options"
        echo ""
        exit 0;;
    *) warning "unknown option \"$1\"." 1>&2
  esac
  shift
done

# add -reuse to std, and -mi to c/cpp
function expand_benches {
  local newb=""
  for bench in $benches; do
    local base=${bench%.*}
    if [[ $bench == *-std\.kk ]]; then
      newb="$newb $base-reuse.kk $bench"  # order matters
    elif [[ $bench == *\.c ]]; then
      newb="$newb $bench $base-mi.c"
    elif [[ $bench == *\.cpp ]]; then
      newb="$newb $bench $base-mi.cpp"
    else
      newb="$newb $bench"
    fi
  done 
  benches=$newb
  echo "expanded benches: $benches"
}

expand_benches

function build_kk { # <bench>
  
  local srcname="$1"
  local base=${1%.*}            # no ext
  local stem=${base##*/}     # dashed dir
  local options="-O2 --no-debug --cc=$ccomp --buildtag=bench --buildname=$stem $kkopts"  
  if [[ $1 == *-std-reuse\.kk ]]; then
    srcname="${1%-std-reuse.kk}-std.kk"    
  fi
  if [[ $1 == *-std\.kk ]]; then
    options="$options --fno-reuse"
  fi
  if ! [ -f "$benchdir/$srcname" ]; then
    info "SKIP $bench ($benchdir/$srcname) -- not found"
  else
    local cmd="$koka $options -i$benchdir $benchdir/$srcname"
    info ""
    info "build: $1: $cmd"
    $cmd
    # "$koka" $options -i$benchdir $benchdir/$srcname
  fi
}

function build_c { # <bench> 
  local srcname="$1"
  local base=${1%.*}
  local stem=${base##*/}
  local options="-O3 -o $coutdir/$stem $copts"
  if [[ $(uname -m) == 'arm64' ]]; then
    options="$options -mcpu=apple-m1"
  else
    options="$options -march=native"
  fi
  if [[ "$1" == *"-mi"* ]]; then
    options="$options $mimalloc_o -I ${mimalloc_usr_local}include/$mimalloc"
    srcname="${1%-mi.c}.c"
  fi
  if ! [ -f "$benchdir/$srcname" ]; then
    info "SKIP $bench ($benchdir/$srcname) -- not found"
  else
    local cmd="$ccomp $options $benchdir/$srcname"
    info ""
    info "build: $1: $cmd"
    $cmd
  fi
}

function build_cpp { # <bench>
  local srcname="$1"
  local base=${1%.*}
  local stem=${base##*/}
  local options="--std=c++17 -O3 -o $cppoutdir/$stem $cppopts"
  if [[ $(uname -m) == 'arm64' ]]; then
    options="$options -mcpu=apple-m1"
  else
    options="$options -march=native"
  fi
  if [[ "$1" == *"-mi"* ]]; then
    options="$options $mimalloc_o -I ${mimalloc_usr_local}include/$mimalloc"
    srcname="${1%-mi.cpp}.cpp"
  fi
  if ! [ -f "$benchdir/$srcname" ]; then
    info "SKIP $bench ($benchdir/$srcname) -- not found"
  else  
    local cmd="$cppcomp $options $benchdir/$srcname"
    info ""
    info "build: $1: $cmd"
    $cmd
  fi
}

function build_all {
  for bench in $benches; do
    if [[ $bench == *\.kk ]]; then
      build_kk $bench $ccomp
    elif [[ $bench == *\.c ]]; then
      build_c $bench 
    elif [[ $bench == *\.cpp ]]; then
      build_cpp $bench 
    else
      warning "define build compiler for $bench"
    fi
  done
}

function run {  #label cmd runidx log runparam
  info ""
  info "run $1, iter $3, cmd: $2"
  local logrun="./log/run.txt"
  $gtime -o $logrun -f "elapsed: %es, user: %Us, sys: %Ss, rss: %Mkb" $2 $5 
  cat $logrun
  # extract elapsed time
  local line=`head -1 $logrun`
  local elapsed=${line#elapsed: }
  elapsed=${elapsed/s,*/}
  local rss=${line#*rss: }
  rss=${rss/kb*/}
  echo "$elapsed $rss" >> "$4"
}

function run_all {
  for bench in $benches; do
    local exe=""
    local prefix=${bench#*\.}
    local base=${bench%\.*} # no extension      
    local stem=${base##*/}  # no directory
    
    if [[ $bench == *\.kk ]]; then
      exe=".koka/${koka_ver}-bench/$ccomp-release/$stem"
    elif [[ $bench == *\.c ]]; then
      exe=".koka/ccomp/$stem"      
    elif [[ $bench == *\.cpp ]]; then
      exe=".koka/cppcomp/$stem"      
    fi

    local cmd="$exe"
    if ! [ -f $exe ]; then
       info "bench $stem: NA (exe not found: $exe)"
    elif [ -z $cmd ]; then
       info "bench $rtem: NA (no command)" # define for ML 
    else
      for runparam in $runparams; do
        local bname="${prefix}__${stem}__$runparam"
        local log="./log/$bname.txt"
        rm -f $log 2> /dev/null   
        for ((runs=1; runs<=$max_runs; runs++)); do
          run $bname $cmd $runs $log $runparam
        done
      done
    fi
  done   
}


basetime=""

function avg { #$1=bname $2=log $3=logbench $4=<kk|c|cpp> $5=benchname $6=<variant> $7=<runparam>
  local median="0.01"
  local stddev="0"
  local rss="0"
  if [ -f "$2" ]; then    
    local median=`sort -n $2 | awk ' { a[i++]=$1; } END { x=int((i+1)/2); if (x < (i+1)/2) print (a[x-1]+a[x])/2; else print a[x-1]; }'`
    if [ "$median" = "0" ]; then
      median="0.01"
    fi
    local stddev=`awk ' { sqrsum += ($1 - '"$median"')^2; } END { print sqrt(sqrsum/NR); }' < $2`
    local rss=`sort -n $2 | awk ' { a[i++]=$2; } END { x=int((i+1)/2); if (x < (i+1)/2) print (a[x-1]+a[x])/2; else print a[x-1]; }'`
    if [ "$basetime" = "" ]; then
      basetime="$median"      
    fi
  fi
  local rmedian=`echo "scale=3; $median / $basetime" | bc`
  local rstddev=`echo "scale=3; $rmedian * $stddev" | bc`
  if [[ $median == Command* ]]; then
    # echo "$1 NA NA NA NA (out of stack)" >> $3
    echo "$4 $5 $6 $7 NA 0.1 0 0.1" >> $3
  else
    # echo "$1 ${median}s ${rmedian}x ${rstddev} ${rss}kb" >> $3
    echo "$4 $5 $6 $7 ${median} ${rmedian} ${rstddev} ${rss}" >> $3
  fi
}

function avg_all {
  local logbench="./log/avg.txt"
  rm -f $logbench 2> /dev/null
  echo "# benchmark variant param elapsed relative stddev rss" >> $logbench
  for benchmark in $benchmarks; do
    for runparam in $runparams; do    
      basetime=""  
      for bench in $benches; do
        local prefix=${bench#*\.}
        local base=${bench%\.*}  # no extension       
        local stem=${base##*/}
        # local bdir=$(echo $base | cut -d'/' -f 1)
        local variant=${stem#*-}   
        local bname=${stem%%-*}
        local label="${prefix}__${stem}__${runparam}"
        local log="./log/$label.txt"
        if [ "$benchmark" = "$bname" ]; then
          avg $label $log $logbench $prefix $bname $variant $runparam
        fi
      done    
      echo "##" >> $logbench
    done
    echo "" >> $logbench
  done
  echo ""
  column -t $logbench
}



#-------------------------------------
# graph with the x ticks for each runparam

function xgraph_variant { # <kk|ml> map <variant> <varianttexname> <avglog> <texdata>
  #            $1 $2  $3   $4    $5        $6         $7       $8    $9
  # log entry: kk map trmc 1000  <elapsed> <relative> <stddev> <rss> <notes>
  awk '
    BEGIN {
      prefix="'"$1"'"
      bench="'"$2"'"
      variant="'"$3"'"
      varianttexname="'"$4"'"
      print "\\pgfplotstableread{"
      print "x y y-error meta"
    }
    $1==prefix && $2==bench && $3==variant {
      if ($1 == "kk" && $3 == "trmc") {
        printf( "%i %0.3f %0.3f {\\absnormlabel{%0.2f}}\n", i++, $6, $7, $5 );
      }
      else if ($6 == 0.1) {
        printf( "%i 0.100 0.000 {\\!\\!out of stack}\n", i++);
      }
      else {
        printf( "%i %0.3f %0.3f {\\normlabel{%0.2f}}\n", i++, ($6>3 ? 3 : $6), $7, $6);
      }
    }  
    END {
      print "}\\datatime" prefix bench varianttexname
      print " "
    }
  ' $5 >> $6
}

function xgraph_all {
  local logbench="./log/avg.txt"
  local texdata="./log/graph.tex"
  echo "\\pgfplotsset{" > $texdata 
  echo "  xticklabels = {" >> $texdata
  #local benchname=""
  #for bench in $benches; do
  #  local bbench=${bench#*\/}  # no directory
  #  benchname=${bbench%\_*}
  #  break
  #done
  for runparam in $runparams; do
    local lab="$runparam"
    if [ "$lab" = "10000" ]; then
      lab="10\\nsep 000"
    elif [ "$lab" = "100000" ]; then
      lab="100\\nsep 000"
    elif [ "$lab" = "1000000" ]; then
      lab="1\\nsep 000\\nsep 000"
    fi
    echo "   \\strut $lab," >> $texdata    
  done
  echo "}}" >> $texdata
  echo " " >> $texdata
  for bench in $benches; do
    local prefix=${bench#*\.}
    local base=${bench%\.*}  # no extension       
    local stem=${base##*\/}  # no directory
    local variant=${stem#*-}   
    local varianttexname="${variant//-/x/}"
    local benchname=${stem%%-*}
    echo "GRAPH $benchname, $variant"    
    xgraph_variant $prefix $benchname $variant $varianttexname $logbench $texdata
  done
  cat $texdata
}


#--------------------------------------
# graph with xtick each benchmark


function graph_variant { # $1=<kk|c|cpp> $2=<variant> $3=<varianttexname> $4=<avglog> $5=<texdata>
  # 
  #            $1 $2  $3   $4    $5        $6         $7       $8    $9
  # log entry: kk map trmc 1000  <elapsed> <relative> <stddev> <rss> <notes>
  awk '
    BEGIN {
      prefix="'"$1"'"
      variant="'"$2"'"
      varianttexname="'"$3"'"
      print "\\pgfplotstableread{"
      print "x y y-error meta"
    }
    $1==prefix && $3==variant {
      if ($1 == "kk" && $3 == "fip") {
        printf( "%i %0.3f %0.3f {\\absnormlabel{%0.2f}}\n", i++, $6, $7, $5 );
      }
      else if ($6 == 0.1) {
        printf( "%i 0.100 0.000 {\\!\\!out of stack}\n", i++);
      }
      else {
        printf( "%i %0.3f %0.3f {\\normlabel{%0.2f}}\n", i++, ($6>3 ? 3 : $6), $7, $6);
      }
    }  
    END {
      print "}\\datatime" prefix varianttexname
      print " "
    }
  ' $4 >> $5
}

function graph_all {
  local logbench="./log/avg.txt"
  local texdata="./log/graph.tex"
  
  echo "\\pgfplotsset{" > $texdata 
  echo "  xticklabels = {" >> $texdata
  for benchmark in $benchmarks; do
    echo "   \\strut $benchmark," >> $texdata
  done  
  echo "}}" >> $texdata
  echo " " >> $texdata

  for variant in $graphvariants; do
    local varianttexname="${variant//-/}"
    graph_variant "kk" $variant $varianttexname $logbench $texdata
    # graph_variant "cpp" $variant $varianttexname $logbench $texdata
    graph_variant "c" $variant $varianttexname $logbench $texdata
  done
  cat $texdata
}


if [ "$do_build" = "yes" ]; then
  build_all
fi

if [ "$do_run" = "yes" ]; then
  run_all
fi  

if [ "$do_avg" = "yes" ]; then
  avg_all
fi  

if [ "$do_graph" = "yes" ]; then
  graph_all
  #xgraph_all
fi
