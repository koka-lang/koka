
#
runparams="100000" # "1 10 100 1000 10000 100000 1000000"
runparams_small="1 10 100 1000"
benchmarks="mtr-rec mtr-td mtr-bu splay-rec splay-td splay-bu zip-td zip-bu rb-rec rb-td rb-bu"
graphvariants="-"  # "td bu"
languages="kk c ml hs cmi" # cp icl

# note: order matters as it is made relative to the first
benches_mtr_rec="mtr/mtr-rec.kk mtr/mtr_rec.icl mtr/mtr-rec.ml mtr/mtr-rec.hs"
benches_mtr_td="mtr/mtr-td.kk mtr/mtr-td.c"
benches_mtr_bu="mtr/mtr-bu.kk  mtr/mtr-bu.c mtr/mtr-bu-rev.c mtr/mtr_bu.icl mtr/mtr-bu.ml mtr/mtr-bu.hs"
benches_splay_rec="splay/splay-rec.kk splay/splay_rec.icl splay/splay-rec.ml splay/splay-rec.hs"
benches_splay_td="splay/splay-td.kk splay/splay-td.c"
benches_splay_bu="splay/splay-bu.kk splay/splay-bu.c splay/splay_bu.icl splay/splay-bu.ml splay/splay-bu.hs"
benches_zip_td="zip/zip-td.kk zip/zip-td.c"
benches_zip_bu="zip/zip-bu.kk zip/zip-bu.c zip/zip_bu.icl zip/zip-bu.ml zip/zip-bu.hs"
benches_rb_rec="rbtree/rb-rec.kk rbtree/rb-rec.ml rbtree/rb-rec.hs"
benches_rb_td="rbtree/rb-td.kk rbtree/rb-td.c"
benches_rb_bu="rbtree/rb-bu.kk rbtree/rb-bu.c rbtree/rb_bu.icl rbtree/rb-bu.ml rbtree/rb-bu.hs"

benches_mtr="$benches_mtr_rec $benches_mtr_td $benches_mtr_bu"
benches_splay="$benches_splay_rec $benches_splay_td $benches_splay_bu"
benches_zip="$benches_zip_td $benches_zip_bu"
benches_rb="$benches_rb_rec $benches_rb_td $benches_rb_bu"
benches_all="$benches_mtr $benches_splay $benches_zip $benches_rb"

# get this by running `stack path | grep local-install-root`` in the koka development directory
# koka_install_dir="/mnt/c/Users/daan/dev/koka/.stack-work/install/x86_64-linux-tinfo6/8f1dbd1b92c17da66792bc77d6f502c989021e266b5032f3d5607fcca4a773e2/8.10.7"
# koka_install_dir="/Users/daan/dev/koka/.stack-work/install/aarch64-osx/b63e887d74237da23db5e39821e14b1f1662540a2b2d9c63219cb143bf61a966/8.10.7"
# koka_install_dir="/Users/anton/orga/phd/koka/.stack-work/install/aarch64-osx/1a7c21de82e435443ed6a5394d51d0409374699330e76b45953b1b5661520371/8.10.7"

# koka_install_dir="/home/daan/dev/koka-v2.4.2/.stack-work/install/x86_64-linux-tinfo6/b1e3fd38737e32c9ae1ce34b0a041070a5540c6539c1d27e80db48e455ea2a13/9.4.5"
# koka_ver="v2.4.2"
# koka_extra_opts=""

koka_ver="v3.1.2"
koka_extra_opts="--no-buildhash"

# if kokainstall does not exist, try to find it from a local koka development directory
if ! [ -d "$koka_install_dir" ]; then
  koka_dev_dir=""
  if [ -d "../../koka" ]; then
    koka_dev_dir="../../koka"
  else
    if [ -d "../../../koka" ]; then
      koka_dev_dir="../../../koka"
    else
      if [ -d "../../../../koka" ]; then
        koka_dev_dir="../../../../koka"
      fi
    fi
  fi
  if [ -d "$koka_dev_dir" ]; then
    pushd "$koka_dev_dir"
    koka_install_dir=`stack path | grep local-install-root`
    koka_install_dir="${koka_install_dir#local-install-root: }"
    popd
  fi
fi

koka="${koka_install_dir}/bin/koka"

echo ""
echo "using koka: $koka"

coutdir=".koka/ccomp"
copts="-lpthread"

cppoutdir=".koka/cppcomp"
cppopts="-lpthread"

cloutdir=".koka/clcomp"
# We use a heap size of 18M, since that is what Haskell
# uses in practice. OCaml uses 10M, Koka 5M and C 4M.
clopts="-ou -h 18M -s 2M -nt -ngc -nst -nr"

mloutdir=".koka/mlcomp"
mlopts=""

hsoutdir=".koka/hscomp"
hsopts=""

mimalloc_o="/usr/local/lib/mimalloc-2.1/mimalloc.o"

gtime="/usr/bin/time"
if command -v "gtime" > /dev/null; then
  gtime=`which gtime`
fi
echo ""

cppcomp="clang++"
ccomp="clang"
clcomp="clm"
mlcomp="ocamlopt"
hscomp="ghc"
benches=""

hascl="no"

kkvariant="release"

kkopts=""
benchdir="."
verbose="no"

do_build="no"
do_run="no"
do_avg="no"
do_graph="no"
graph_abs="no"
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
ensure_dir ".koka/mlcomp"
ensure_dir ".koka/hscomp"

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

    all|allb) benches="$benches_all";;

    mtrrec)  benches="$benches $benches_mtr_rec";;
    splayrec)  benches="$benches $benches_splay_rec";;
    rbrec)  benches="$benches $benches_rb_rec";;
    allrec) benches="$benches $benches_mtr_rec $benches_splay_rec $benches_rb_rec";;

    mtrtd)  benches="$benches $benches_mtr_td";;
    splaytd)  benches="$benches $benches_splay_td";;
    ziptd) benches="$benches $benches_zip_td";;
    rbtd) benches="$benches $benches_rb_td";;
    alltd) benches="$benches $benches_mtr_td $benches_splay_td $benches_zip_td $benches_rb_td";;

    mtrbu)  benches="$benches $benches_mtr_bu";;
    splaybu)  benches="$benches $benches_splay_bu";;
    zipbu) benches="$benches $benches_zip_bu";;
    rbbu) benches="$benches $benches_rb_bu";;
    allbu) benches="$benches $benches_mtr_bu $benches_splay_bu $benches_zip_bu $benches_rb_bu";;

    mtr)  benches="$benches $benches_mtr";;
    splay)  benches="$benches $benches_splay";;
    zip) benches="$benches $benches_zip";;
    rb) benches="$benches $benches_rb";;

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
    nounroll) kkopts="$kkopts --fno-unroll";;
    kkdebug) kkopts="-O0 --debug --fallocstats"
             kkvariant="allocstats-debug";;
    parent) copts="$copts -DADDPARENT=1"
            cppopts="$cppopts -DADDPARENT=1";;
    allocstats)
      kkopts="$kkopts --fallocstats"
      kkvariant="allocstats-release"
      copts="$copts -DALLOCSTATS=1"
      cppopts="$cppopts -DALLOCSTATS=1";;

    koka)  koka="$flag_arg";;
    ocamlopt) ocamlopt="$flag_arg";;

    kkopts) kkopts="$kkopts $flag_arg";;
    copts) copts="$copts $flag_arg";;
    cppopts) cppopts="$cppopts $flag_arg";;

    small) runparams="$runparams_small";;

    gabs) graph_abs="yes";;

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

if command -v "$clcomp" > /dev/null; then
  hascl="yes"
else
  info "clean compiler not found: $clcomp"
fi


# add -reuse to std, and -mi to c/cpp
function expand_benches {
  local newb=""
  for bench in $benches; do
    local base=${bench%.*}
    if [[ $bench == *-std\.kk ]]; then
      newb="$newb $base-reuse.kk $bench"
    elif [[ $bench == *\.c ]]; then
      newb="$newb $bench $base-p.c $base-mi.c"  # parent pointer and mimalloc
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
  local options="-v0 -O2 --no-debug --cc=$ccomp $koka_extra_opts --buildtag=bench --buildname=$stem $kkopts"
  if [[ $(uname -m) == 'arm64' ]]; then
    options="$options --ccopts=-mcpu=apple-m1"
  else
    options="$options --ccopts=-march=native --ccopts=-mtune=native -O3"
  fi
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
  local options="-O3 -DNDEBUG=1 -o $coutdir/$stem $copts"
  if [[ $(uname -m) == 'arm64' ]]; then
    options="$options -mcpu=apple-m1"
  else
    options="$options -march=native"
  fi
  if [[ "$1" == *"-mi"* ]]; then
    options="$options $mimalloc_o -I ${mimalloc_usr_local}include/$mimalloc"
    srcname="${1%-mi.c}.c"
  fi
  if [[ "$1" == *"-p"* ]]; then
    options="$options -DADDPARENT=1 $mimalloc_o -I ${mimalloc_usr_local}include/$mimalloc"
    srcname="${1%-p.c}.c"
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

# Clean needs to be linked against the clean_env.c file that
# implements our random number generator and the CommandLine.icl file.
# Both are in the mtr/ directory. Unfortunately, the compiler can only
# reliably find files in the current directory and does not understand
# "-" in a module name. For this reason, the clean files use "_" and we
# enter the source directory for compilation.
function build_cl { # <bench>
  local srcname="$1"
  local base=${1%.*}
  local stem=${base##*/}
  local dir=${1%/*}
  local options="-l clean_env.o -I ../mtr $clopts"
  if ! [ -f "$benchdir/$srcname" ]; then
    info "SKIP $bench ($benchdir/$srcname) -- not found"
  else
    pushd "$dir"
    # Even on Mac M1, we have to use x86_64 to be able to link to Clean
    local cmd1="$ccomp -arch x86_64 -c ../mtr/clean_env.c"
    local cmd2="$clcomp $options $stem"
    info ""
    info "build: $1: $cmd1; $cmd2"
    $cmd1
    $cmd2
    if [ -f "clean_env.o" ]; then
      rm -f "clean_env.o"
    fi
    if [ -d "Clean System Files" ]; then
      rm -rf "Clean System Files"
    fi
    popd
    mkdir -p $cloutdir
    local stemm=${stem//_/-}
    mv "$dir/a.out" $cloutdir/$stemm
  fi
}

function build_ml { # <bench>
  local srcname="$1"
  local base=${1%.*}
  local stem=${base##*/}
  local options="-O2 -o $mloutdir/$stem $mlopts"
  if ! [ -f "$benchdir/$srcname" ]; then
    info "SKIP $bench ($benchdir/$srcname) -- not found"
  else
    local cmd="$mlcomp $options $benchdir/$srcname"
    info ""
    info "build: $1: $cmd"
    $cmd
    local basename="$benchdir/$base"
    if [ -f "$basename.o" ]; then
      rm -f "$basename.o"
    fi
    if [ -f "$basename.cmi" ]; then
      rm -f "$basename.cmi"
    fi
    if [ -f "$basename.cmx" ]; then
      rm -f "$basename.cmx"
    fi
  fi
}

function build_hs { # <bench>
  local srcname="$1"
  local base=${1%.*}
  local stem=${base##*/}
  local srcdir=${1%%/*}
  local options="-i$srcdir -O2 -o $hsoutdir/$stem $hsopts"
  if ! [ -f "$benchdir/$srcname" ]; then
    info "SKIP $bench ($benchdir/$srcname) -- not found"
  else
    touch  $benchdir/$srcname
    local cmd="$hscomp $options $benchdir/$srcname"
    info ""
    info "build: $1: $cmd"
    $cmd
    local basename="$benchdir/$base"
    if [ -f "$basename.o" ]; then
      rm -f "$basename.o"
    fi
    if [ -f "$basename.hi" ]; then
      rm -f "$basename.hi"
    fi
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
    elif [[ $bench == *\.icl ]]; then
      if [ "$hascl" == "yes" ]; then
        build_cl $bench
      fi
    elif [[ $bench == *\.ml ]]; then
      build_ml $bench
    elif [[ $bench == *\.hs ]]; then
      build_hs $bench
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
      exe=".koka/${koka_ver}-bench/$ccomp-$kkvariant/$stem"
    elif [[ $bench == *\.c ]]; then
      exe=".koka/ccomp/$stem"
      if [[ "$stem" == *"-mi" ]]; then
        prefix="cmi"
        stem="${stem%-mi}"
      fi
      if [[ "$stem" == *"-p" ]]; then
        prefix="cp"
        stem="${stem%-p}"
      fi
    elif [[ $bench == *\.cpp ]]; then
      exe=".koka/cppcomp/$stem"
    elif [[ $bench == *\.icl ]]; then
      if [ "$hascl" == "yes" ]; then
        local stemm=${stem//_/-}
        exe=".koka/clcomp/$stemm"
      fi
    elif [[ $bench == *\.ml ]]; then
      exe=".koka/mlcomp/$stem"
    elif [[ $bench == *\.hs ]]; then
      exe=".koka/hscomp/$stem"
    fi

    local cmd="$exe"
    if ! [ -f $exe ]; then
       info "bench $prefix $stem: NA (exe not found: $exe)"
    elif [ -z $cmd ]; then
       info "bench $prefix $stem: NA (no command)" # define for ML
    else
      for runparam in $runparams; do
        local bname="${prefix}__${stem//_/-}__$runparam"
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
    if [[ $median == "0.01" ]]; then
      echo "$4 $5 $6 $7 NA 0 0 0" >> $3
    else
      # echo "$1 ${median}s ${rmedian}x ${rstddev} ${rss}kb" >> $3
      echo "$4 $5 $6 $7 ${median} ${rmedian} ${rstddev} ${rss}" >> $3
    fi
  fi
}

function avg_all {
  local logbench="./log/avg.txt"
  rm -f $logbench 2> /dev/null
  echo "# benchmark variant param elapsed relative stddev rss" >> $logbench
  for benchmark in $benchmarks; do
    basetime=""
    for lang in $languages; do
      for runparam in $runparams; do
        local prefix="$lang"
        local stem="$benchmark"
        local base="$stem"
        if [[ $graphvariants = "-" ]]; then
          local variant="-"   # ${stem#*-}
          local bname="$stem" # ${stem%%-*}
        else
          local variant="${stem#*-}"
          local bname="${stem%%-*}"
        fi
        local label="${prefix}__${stem}__${runparam}"
        local log="./log/$label.txt"
        echo "$bname"
        avg $label $log $logbench $prefix $bname $variant $runparam
      done
    done
    echo "##" >> $logbench
  done
  echo ""
  column -t $logbench
}


#--------------------------------------
# graph with xtick each benchmark

# show relative time
function graph_variant { # $1=<kk|c|cpp|...> $2=<variant> $3=<varianttexname> $4=<avglog> $5=<texdata>
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
      if ($1 == "kk") {
        printf( "%i %0.3f %0.3f {\\absnormlabel{%0.2f}}\n", i++, $6, $7, $5 );
      }
      else if ($6 == 0) {
        printf( "%i 0.100 0.000 {\\!\\!NA}\n", i++);
      }
      else if ($6 == 0.1) {
        printf( "%i 0.100 0.000 {\\!\\!out of stack}\n", i++);
      }
      else {
        printf( "%i %0.3f %0.3f {\\normlabel{%0.2f}}\n", i++, ($6>3.25 ? 3.25 : $6), $7, $6);
      }
    }
    END {
      print "}\\datatime" prefix varianttexname
      print " "
    }
  ' $4 >> $5
}

# show absolute time
function graph_abs_variant { # $1=<kk|c|cpp|...> $2=<variant> $3=<varianttexname> $4=<avglog> $5=<texdata>
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
      if ($1 == "kk") {
        printf( "%i %0.3f %0.3f {\\absnormlabel{%0.2f}}\n", i++, $5, $7, $5 );
      }
      else if ($6 == 0) {
        printf( "%i 0.100 0.000 {\\!\\!NA}\n", i++);
      }
      else if ($6 == 0.1) {
        printf( "%i 0.100 0.000 {\\!\\!out of stack}\n", i++);
      }
      else {
        printf( "%i %0.3f %0.3f {\\perclabel{%i}}\n", i++, ($5>3.25 ? 3.25 : $5), $7, 100*$6);
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
    if [[ $variant = "-" ]]; then
      local varianttexname=""
    else
      local varianttexname="${variant//-/}"
    fi
    for lang in $languages; do
      if [ "$graph_abs" = "yes" ]; then
        graph_abs_variant $lang $variant "$varianttexname" $logbench $texdata
      else
        graph_variant $lang $variant "$varianttexname" $logbench $texdata
      fi
    done
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
