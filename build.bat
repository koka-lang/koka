@echo off

pushd %~dp0

cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -S cruntime -B cruntime/out/msvc-x64/Debug
cmake --build cruntime/out/msvc-x64/Debug --config Debug
stack build

echo Run with: stack exec koka -- --target=c --core --checkcore -ilib -itest/algeff -itest/cgen -itest/parc -itest/lib
echo Be sure to run `:f std/core/types` and `:f std/core`

popd
