rem Downloading koka binary distribution..
curl -L -o %TEMP%\koka-dist.tar.gz https://github.com/koka-lang/koka/releases/download/v2.0.8/koka-v2.0.8-clang11-win-x86_64.tar.gz
mkdir %APPDATA%\local
rem Installing to: %APPDATA%\local
rem Unpacking    : %TEMP%\koka-dist.tar.gz
tar -xzf %TEMP%\koka-dist.tar.gz -C %APPDATA%\local
rem Done.
rem -----------------------------------------------------------------------
rem Add "%APPDATA%\local\bin" to your PATH environment variable to find koka.
rem -----------------------------------------------------------------------
