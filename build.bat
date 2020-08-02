@echo off

pushd %~dp0

cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=kklib/out/install -S kklib -B kklib/out/debug
cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=kklib/out/install -S kklib -B kklib/out/release

cmake --build kklib/out/debug --config Debug
cmake --build kklib/out/release --config Debug

cmake --install kklib/out/debug
cmake --install kklib/out/release

stack build

stack exec koka -- --target=c -c -ilib -l std/core/types
stack exec koka -- --target=c -c -ilib -l std/core

stack exec koka -- --target=c --outdir=out/test -c -ilib -l std/core/types
stack exec koka -- --target=c --outdir=out/test -c -ilib -l std/core

echo Run with: stack exec koka -- --target=c --core --checkcore -ilib -itest/algeff -itest/cgen -itest/parc -itest/lib
echo To test PARC, run: stack test --test-arguments="--match /parc/"
echo Set CC to the C compiler you want Koka to use.

popd
