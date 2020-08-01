@echo off

pushd %~dp0

cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -S cruntime -B cruntime/out/msvc-x64/Debug
cmake --build cruntime/out/msvc-x64/Debug --config Debug

stack build

stack exec koka -- --target=c --outdir=out/test -c -ilib -l std/core/types
stack exec koka -- --target=c --outdir=out/test -c -ilib -l std/core

echo Run with: stack exec koka -- --target=c --core --checkcore -ilib -itest/algeff -itest/cgen -itest/parc -itest/lib
echo To test PARC, run: stack test --test-arguments="--match /parc/"

popd
