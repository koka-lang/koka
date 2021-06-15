@echo off
REM ------------------------------------------------------------------
REM Installation script for Koka; use -h to see command line options.
REM ------------------------------------------------------------------

set _KOKA_VERSION=v2.1.7
set _KOKA_PREFIX=%LOCALAPPDATA%\local
set _KOKA_UNINSTALL=N
set _KOKA_HELP=N
set _KOKA_FORCE=N
set _KOKA_DIST_SOURCE=
set _KOKA_DIST_SOURCE_URL=
set _KOKA_IEXPRESS=N
set _KOKA_PREV_VERSION=
set _KOKA_PREV_PREFIX=

set _CLANG_VERSION=12.0.0
set _CLANG_INSTALL_BASE=LLVM-%_CLANG_VERSION%-win64.exe
set _CLANG_INSTALL=%TEMP%\%_CLANG_INSTALL_BASE%
set _CLANG_INSTALL_URL=https://github.com/llvm/llvm-project/releases/download/llvmorg-%_CLANG_VERSION%/%_CLANG_INSTALL_BASE%
set _CLANG_INSTALL_SHA256=8426d57f2af2bf07f80014bfd359e87ed10f5521a236a10cfe9fc4870d1b1b25

REM check if %LOCALAPPDATA% was not empty
if "%_KOKA_PREFIX%" == "\local" (set _KOKA_PREFIX=c:\usr\local)

REM process arguments
:argparse
if "%~1" == "" goto done_args
  if "%~1" == "-u" (
    set _KOKA_UNINSTALL=Y
    goto boolflag
  )
  if "%~1" == "--uninstall" (
    set _KOKA_UNINSTALL=Y
    goto boolflag
  )
  if "%~1" == "-h" (
    set _KOKA_HELP=Y
    goto boolflag
  )
  if "%~1" == "--help" (
    set _KOKA_HELP=Y
    goto boolflag
  )
  if "%~1" == "-f" (
    set _KOKA_FORCE=Y
    goto boolflag
  )
  if "%~1" == "--force" (
    set _KOKA_FORCE=Y
    goto boolflag
  )
  if "%~1" == "--iexpress" (
    set _KOKA_IEXPRESS=Y
    goto boolflag
  )
  if "%~1" == "-v"        (set _KOKA_VERSION=%~2)
  if "%~1" == "--version" (set _KOKA_VERSION=%~2)
  if "%~1" == "-p"        (set _KOKA_PREFIX=%~2)
  if "%~1" == "--prefix"  (set _KOKA_PREFIX=%~2)
  if "%~1" == "-b"        (set _KOKA_DIST_SOURCE=%~2)
  if "%~1" == "--bundle"  (set _KOKA_DIST_SOURCE=%~2)
  if "%~1" == "--url"     (set _KOKA_DIST_SOURCE_URL=%~2)  
shift
:boolflag
shift
goto argparse
:done_args 

REM ---------------------------------------------------------
REM Defaults
REM ---------------------------------------------------------

set _KOKA_ARCH=x64
if "%_KOKA_VERSION%" LEQ "v2.1.6" (set _KOKA_ARCH=amd64)

if "%_KOKA_DIST_SOURCE_URL%" == "" (
  set _KOKA_DIST_SOURCE_URL=https://github.com/koka-lang/koka/releases/download/%_KOKA_VERSION%/koka-%_KOKA_VERSION%-windows-%_KOKA_ARCH%.tar.gz
)

REM ---------------------------------------------------------
REM Detect previous version
REM ---------------------------------------------------------

where /q koka
if errorlevel 1 goto prev_none
for /F "tokens=*" %%x in ('where koka 2^> nul ^| find "\bin\koka.exe"') do (set _KOKA_PREV_PREFIX=%%x)
if "%_KOKA_PREV_PREFIX%" == "" goto prev_none
set _KOKA_PREV_PREFIX=%_KOKA_PREV_PREFIX:\bin\koka.exe=%
for /F "tokens=*" %%x in ('koka --version 2^> nul ^| find "version: "') do (set _KOKA_PREV_VERSION=%%x)
if "%_KOKA_PREV_VERSION%" NEQ "" (set _KOKA_PREV_VERSION=v%_KOKA_PREV_VERSION:version: =%)

:prev_none

REM ---------------------------------------------------------
REM Mode
REM ---------------------------------------------------------

if "%_KOKA_HELP%" == "Y"       goto help
if "%_KOKA_UNINSTALL%" == "Y"  goto uninstall
if "%_KOKA_DIST_SOURCE%" == "" goto download 
goto unpack


REM ---------------------------------------------------------
REM Help
REM ---------------------------------------------------------
:help

echo command:
echo   install-koka.bat [options]
echo.
echo options:
echo   -f, --force              continue without prompting
echo   -u, --uninstall          uninstall koka (%_KOKA_VERSION%)
echo   -p, --prefix=^<dir^>       prefix directory (%_KOKA_PREFIX%)
echo   -b, --bundle=^<file^|url^>  full bundle location (%_KOKA_DIST_SOURCE%)
echo   --url=^<url^>              download url (%_KOKA_DIST_SOURCE_URL%)
echo   --version=^<ver^>          version tag (%_KOKA_VERSION%)
echo.
goto end


REM ---------------------------------------------------------
REM Install
REM ---------------------------------------------------------

:download

set _KOKA_DIST_SOURCE=%TEMP%\koka-%_KOKA_VERSION%-windows.tar.gz
  
echo Downloading: %_KOKA_DIST_SOURCE_URL%
curl --proto =https --tlsv1.2 -f -L -o "%_KOKA_DIST_SOURCE%"  "%_KOKA_DIST_SOURCE_URL%"
if errorlevel 1 (
  echo "curl error: %ERRORLEVEL%"
  goto end
)

:unpack
echo.
echo Installing to prefix: %_KOKA_PREFIX%
if not exist %_KOKA_PREFIX% (
  mkdir "%_KOKA_PREFIX%"
)

echo - unpacking..
tar -xzf "%_KOKA_DIST_SOURCE%" -C "%_KOKA_PREFIX%"
if errorlevel 1 (
  echo "Unpacking error: %ERRORLEVEL%"
  goto end
)

echo - install pre-compiled libraries to: ^<prefix^>\lib\koka\%_KOKA_VERSION%
echo - install source libraries to      : ^<prefix^>\share\koka\%_KOKA_VERSION%
echo - install executable to            : ^<prefix^>\bin\koka.exe
echo - install symlink to               : ^<prefix^>\bin\koka-%_KOKA_VERSION%.exe
copy /B /Y "%_KOKA_PREFIX%\bin\koka.exe" "%_KOKA_PREFIX%\bin\koka-%_KOKA_VERSION%.exe" > nul


REM -----------------------------------------------------------------
REM Install: set PATH environment variable.
REM Note: we need powershell to set the path globally as 
REM the `setx` command cuts of environment values at 1024 characters!
REM -----------------------------------------------------------------

echo "%PATH%" | find "%_KOKA_PREFIX%\bin" >nul
if not errorlevel 1 goto done_env

REM Prevent duplicate semicolon
set _KOKA_SEMI=;
if "%PATH:~-1%"==";" (set _KOKA_SEMI=)
set PATH=%PATH%%_KOKA_SEMI%%_KOKA_PREFIX%\bin

where /q powershell
if not errorlevel 1 (
  echo.
  set _koka_answer=Y
  if "%_KOKA_FORCE%" NEQ "Y" (
    set /p "_koka_answer=Add the koka binary directory to the search PATH? [Yn] " 
  )
  if /i "%_koka_answer:~,1%" == "N" goto done_env

  echo - add binary directory to the user PATH environment variable.
  powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "[Environment]::SetEnvironmentVariable('PATH',\""$([Environment]::GetEnvironmentVariable('PATH','User'))%_KOKA_SEMI%%_KOKA_PREFIX%\bin\"",'User');"
  if not errorlevel 1 goto done_env
)
echo.
echo Please add "%_KOKA_PREFIX%\bin" to your PATH environment variable.
echo.

:done_env

REM ---------------------------------------------------------
REM Editor support
REM ---------------------------------------------------------

if exist "%USERPROFILE%\.atom\packages" (
  echo - install atom editor support
  if not exist "%USERPROFILE%\.atom\packages\language-koka" (
    mkdir "%USERPROFILE%\.atom\packages\language-koka"
  )
  xcopy /Y /Q /S "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%\contrib\atom\*" "%USERPROFILE%\.atom\packages\language-koka" > nul
  set  "koka_editor=atom %%f:%%l:%%c"
  setx koka_editor "atom %%f:%%l:%%c" > nul
)

where /Q code
if errorlevel 1 goto done_vscode

echo - install vscode editor support
code --list-extensions | find "koka-lang.language-koka" > nul
if not errorlevel 1 (
  echo uninstall vscode ext
  code --uninstall-extension koka-lang.language-koka > nul
)
cmd /C "code --force --install-extension koka.language-koka"
if errorlevel 1 (
  echo Could not install VS Code editor support
  goto done_vscode
) 

set  "koka_editor=code --goto %%f:%%l:%%c"
setx koka_editor "code --goto %%f:%%l:%%c" > nul

:done_vscode

where /Q emacs
if errorlevel 1 goto done_emacs
echo - emacs syntax mode can be found at: %_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%\contrib\emacs

:done_emacs

REM ---------------------------------------------------------
REM Uninstall previous version
REM ---------------------------------------------------------

if "%_KOKA_PREV_PREFIX%" == "" goto done_install

REM always delete a previous koka.exe _if installed at a different prefix_ on the PATH 
REM (so the newly installed koka gets found instead of an older one)
if "%_KOKA_PREV_PREFIX%" NEQ "%_KOKA_PREFIX%" (
  echo "%PATH%" | find "%_KOKA_PREV_PREFIX%\bin" >nul
  if not errorlevel 1 (
    if exist "%_KOKA_PREV_PREFIX%\bin\koka.exe" (
      del /Q "%_KOKA_PREV_PREFIX%\bin\koka.exe"
    )
  )
)

REM Did we update in place?
if "%_KOKA_PREV_PREFIX%,%_KOKA_PREV_VERSION%" == "%_KOKA_PREFIX%,%_KOKA_VERSION%" (
  echo Updated koka version %_KOKA_VERSION% in-place
  goto done_install
)


if not exist "%_KOKA_PREV_PREFIX%\lib\koka\%_KOKA_PREV_VERSION%" goto done_install

echo.
set _koka_answer=N
if "%_KOKA_FORCE%" NEQ "Y" (
  set /p "_koka_answer=Found previous koka installation %_KOKA_PREV_VERSION%, Uninstall? [yN] " 
)
if /i "%_koka_answer:~,1%" NEQ "Y" goto done_install

:uninstallprev
echo Uninstalling previous koka installation %_KOKA_PREV_VERSION%..
if exist "%_KOKA_PREV_PREFIX%\bin\koka-%_KOKA_PREV_VERSION%.exe" (
  echo - remove executable            : ^<prefix^>\bin\koka-%_KOKA_PREV_VERSION%.exe  
  del /Q "%_KOKA_PREV_PREFIX%\bin\koka-%_KOKA_PREV_VERSION%.exe"
)
echo - remove pre-compiled libraries: ^<prefix^>\lib\koka\%_KOKA_PREV_VERSION%
rmdir /S /Q "%_KOKA_PREV_PREFIX%\lib\koka\%_KOKA_PREV_VERSION%"
echo - remove source libraries      : ^<prefix^>\share\koka\%_KOKA_PREV_VERSION%
rmdir /S /Q "%_KOKA_PREV_PREFIX%\share\koka\%_KOKA_PREV_VERSION%"
goto done_install


REM ---------------------------------------------------------
REM Uninstall
REM ---------------------------------------------------------

:uninstall
echo Uninstalling %_KOKA_VERSION% from prefix: %_KOKA_PREFIX%

if not exist "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%" (
  echo Cannot find koka version %_KOKA_VERSION% at %_KOKA_PREFIX%
  echo Done. 
  goto end
)

set _koka_answer=N
if "%_KOKA_FORCE%" NEQ "Y" (
  set /p "_koka_answer=Are you sure? [yN] " 
)
if /i "%_koka_answer:~,1%" NEQ "Y" goto end

if exist "%_KOKA_PREFIX%\bin\koka-%_KOKA_VERSION%.exe" (
  echo - remove executable            : ^<prefix^>\bin\koka.exe
  fc /LB1 "%_KOKA_PREFIX%\bin\koka.exe" "%_KOKA_PREFIX%\bin\koka-%_KOKA_VERSION%.exe" > nul 2> nul
  if not errorlevel 1 (del /Q "%_KOKA_PREFIX%\bin\koka.exe")
  echo - remove executable            : ^<prefix^>\bin\koka-%_KOKA_VERSION%.exe
  del /Q "%_KOKA_PREFIX%\bin\koka-%_KOKA_VERSION%.exe"
)
echo - remove pre-compiled libraries: ^<prefix^>\lib\koka\%_KOKA_VERSION%
rmdir /S /Q "%_KOKA_PREFIX%\lib\koka\%_KOKA_VERSION%"
echo - remove source libraries      : ^<prefix^>\share\koka\%_KOKA_VERSION%
rmdir /S /Q "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%"

echo Done.

goto end


REM ---------------------------------------------------------
REM Install completed
REM ---------------------------------------------------------
:done_install


REM ---------------------------------------------------------
REM Install clang if needed
REM ---------------------------------------------------------

where /q clang-cl
if not errorlevel 1 goto done_clang

echo.
echo -----------------------------------------------------------------------
echo Cannot find the clang-cl compiler. 
echo A C compiler is required for Koka to function.

set _koka_answer=Y
if "%_KOKA_FORCE%" NEQ "Y" (
  set /p "_koka_answer=Would you like to download and install clang %_CLANG_VERSION% for Windows? [Yn] " 
)
if /i "%_koka_answer:~,1%" NEQ "Y" (
  echo Canceled automatic install.
  echo.
  goto clang_showurl
)

echo.
echo Downloading clang over https from: 
echo  %_CLANG_INSTALL_URL%
curl --proto =https --tlsv1.2 -f -L -o "%_CLANG_INSTALL%" "%_CLANG_INSTALL_URL%"
if errorlevel 1 goto clang_showurl

if "%_CLANG_INSTALL_SHA256%" NEQ "" (
  echo Verifying sha256 hash ...
  timeout /T 1 > nul  
  CertUtil -hashfile "%_CLANG_INSTALL%" sha256 | find "%_CLANG_INSTALL_SHA256%" > nul
  if errorlevel 1 (
    echo Installation of %_CLANG_INSTALL% is canceled as it does not match the
    echo expected sha256 signature: %_CLANG_INSTALL_SHA256%
    echo.
    goto clang_showurl
  )
  echo Ok.
  timeout /T 1 > nul  
)

echo.
echo Installing clang ...   (%_CLANG_INSTALL%)
"%_CLANG_INSTALL%"
if not errorlevel 1 (
  set "PATH=%PATH%;C:\Program Files\LLVM\bin"
)
del /Q "%_CLANG_INSTALL%"
goto done_clang

:clang_showurl
echo Please install clang for Windows manually from: https://llvm.org/builds

:done_clang

REM ---------------------------------------------------------
REM End
REM ---------------------------------------------------------

echo.
echo -----------------------------------------------------------------------
echo Installed koka %_KOKA_VERSION% to: %_KOKA_PREFIX%\bin\koka
echo.

if "%_KOKA_IEXPRESS%" == "Y" (
  set /p "_koka_answer=Press <enter> to finish installation.." 
) else (
  echo Type 'koka' to enter the interactive compiler.
)
echo.

:end

REM clean environment
set _KOKA_VERSION=
set _KOKA_PREFIX=
set _KOKA_UNINSTALL=
set _KOKA_HELP=
set _KOKA_FORCE=
set _KOKA_DIST_SOURCE=
set _KOKA_DIST_SOURCE_URL=
set _KOKA_ARCH=
set _KOKA_PREV_PREFIX=
set _KOKA_PREV_VERSION=
set _KOKA_SEMI=
set _CLANG_VERSION=
set _CLANG_INSTALL_BASE=
set _CLANG_INSTALL=
set _CLANG_INSTALL_URL=
set _CLANG_INSTALL_SHA256=
set _KOKA_IEXPRESS=
set _koka_answer=

