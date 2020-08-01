@echo off

pushd %~dp0

cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=cruntime/out/install -S cruntime -B cruntime/out/Debug
cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=cruntime/out/install -S cruntime -B cruntime/out/Release

cmake --build cruntime/out/Debug --config Debug
cmake --build cruntime/out/Release --config Debug

cmake --install cruntime/out/Debug
cmake --install cruntime/out/Release

stack build

stack exec koka -- --target=c -c -ilib -l std/core/types
stack exec koka -- --target=c -c -ilib -l std/core

stack exec koka -- --target=c --outdir=out/test -c -ilib -l std/core/types
stack exec koka -- --target=c --outdir=out/test -c -ilib -l std/core

echo Run with: stack exec koka -- --target=c --core --checkcore -ilib -itest/algeff -itest/cgen -itest/parc -itest/lib
echo To test PARC, run: stack test --test-arguments="--match /parc/"

popd
