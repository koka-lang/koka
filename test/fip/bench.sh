

# list sizes
runparams="1 10 100 1000 10000 100000 1000000"
runparams_small="1 10 100 1000"
dirs="tmap rbtree finger sort"

benches_tmapkk="tmap/tmap_std.kk tmap/tmap_fip.kk tmap/tmap_std_noreuse.kk tmap/tmap_fip_noreuse.kk"
benches_tmapc="tmap/tmap_std_mimalloc.c tmap/tmap_fip_mimalloc.c tmap/tmap_std.c tmap/tmap_fip.c"
benches_rbtreekk="rbtree/rbtree_icfp.kk rbtree/rbtree_std.kk rbtree/rbtree_fip.kk rbtree/rbtree_std_noreuse.kk rbtree/rbtree_fip_noreuse.kk"
benches_rbtreec="rbtree/rbtree_clrs_mimalloc.c rbtree/rbtree_clrs_full_mimalloc.c rbtree/rbtree_clrs.c rbtree/rbtree_clrs_full.c"
benches_sortkk="sort/sort_merge_std.kk sort/sort_merge_fip.kk sort/sort_quick_std.kk sort/sort_quick_fip.kk"
benches_fingerkk="finger/finger_std.kk finger/finger_fip.kk"
benches_all="$benches_tmapkk $benches_tmapc $benches_rbtreekk $benches_rbtreec $benches_fingerkk $benches_sortkk"

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
copts=""

gtime="/usr/bin/time"
if command -v "gtime"; then
  gtime=`which gtime`
fi

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
    tmapkk)   benches="$benches $benches_tmapkk";;
    rbtreekk) benches="$benches $benches_rbtreekk";;
    sortkk) benches="$benches $benches_sortkk";;
    fingerkk) benches="$benches $benches_fingerkk";;
    allc)     benches="$benches $benches_tmapc $benches_rbtreec";;
    tmapc)   benches="$benches $benches_tmapc";;
    rbtreec) benches="$benches $benches_rbtreec";;

    ccomp) ccomp="$flag_arg";;
    gcc)   ccomp="gcc";;
    clang) ccomp="clang";;

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
        exit 0;;
    *) warning "unknown option \"$1\"." 1>&2
  esac
  shift
done

function build_kk { # <bench> 
  local options="-O2 --no-debug --cc=$ccomp --buildtag=bench $kkopts"
  if [[ "$1" == *"noreuse.kk"* ]]; then
    options="$options --fno-reuse"
  fi
  info ""
  info "build: $1, ($options)"
  "$koka" $options -i$benchdir $benchdir/$1  
}

function build_c { # <bench> 
  local base=${1%.*}
  local dbase=${base//\//_}
  local options="-O3 -o $coutdir/$dbase $copts"
  if [[ $(uname -m) == 'arm64' ]]; then
    options="$options -mcpu=apple-m1"
  else
    options="$options -march=native"
  fi
  if [[ "$1" == *"mimalloc.c"* ]]; then
    options="$options -L /usr/local/lib/mimalloc-2.0 -I /usr/local/include/mimalloc-2.0 -lmimalloc"
  fi
  info ""
  info "build: $1, ($options)"
  "$ccomp" $options $benchdir/$1  
}

function build_all {
  for bench in $benches; do
    if ! [ -f "$benchdir/$bench" ]; then
      info "skip $bench -- not found"
    elif [[ $bench == *\.kk ]]; then
      build_kk $bench $ccomp
    elif [[ $bench == *\.c ]]; then
      build_c $bench 
    else
      warning "define build compiler for $bench"
    fi
  done
}

function run {  #bname cmd runidx log runparam
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
    local dbase=${base//\//_}
    
    if [[ $bench == *\.kk ]]; then
      exe=".koka/${koka_ver}-bench/$ccomp-release/$dbase"
    elif [[ $bench == *\.c ]]; then
      exe=".koka/ccomp/$dbase"      
    fi

    local cmd="$exe"
    if ! [ -f $exe ]; then
       info "bench $base: NA (exe not found: $exe)"
    elif [ -z $cmd ]; then
       info "bench $base: NA (no command)" # define for ML 
    else
      for runparam in $runparams; do
        local bname="${prefix}_${dbase}_$runparam"
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

function avg { #bname log logbench $4=<kk|ml> map <variant> <runparam>
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
  for dir in $dirs; do
    for runparam in $runparams; do    
      basetime=""      
      for bench in $benches; do
        local prefix=${bench#*\.}
        local base=${bench%\.*}  # no extension       
        local dbase=${base//\//_}
        local bdir=$(echo $base | cut -d'/' -f 1)
        local variant=${base#*\_}   
        local bname="${prefix}_${dbase}_${runparam}"
        local log="./log/$bname.txt"
        if [ "$dir" = "$bdir" ]; then
          avg $bname $log $logbench $prefix $dir $variant $runparam
        fi
      done    
      echo "##" >> $logbench
    done
    echo "" >> $logbench
  done
  echo ""
  echo "# benchmark        elapsed  relat.  stddev rss"
  column -t $logbench
}

function graph_variant { # <kk|ml> map <variant> <avglog> <texdata>
  #            $1 $2  $3   $4    $5        $6         $7       $8    $9
  # log entry: kk map trmc 1000  <elapsed> <relative> <stddev> <rss> <notes>
  awk '
    BEGIN {
      prefix="'"$1"'"
      bench="'"$2"'"
      variant="'"$3"'"
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
      print "}\\datatime" prefix bench variant
      print " "
    }
  ' $4 >> $5
}

function graph_all {
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
    local bbench=${base#*\/}  # no directory
    local variant=${bbench#*\_}   
    local benchname=${bbench%\_*}
    # echo "$benchname, $variant"    
    graph_variant $prefix $benchname $variant $logbench $texdata
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
fi
