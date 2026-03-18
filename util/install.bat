@echo off
rem ------------------------------------------------------------------
rem Installation script for Koka; use -h to see command line options.
rem ------------------------------------------------------------------

setlocal
set KOKA_VERSION=v3.2.3
set KOKA_PREFIX=%LOCALAPPDATA%\koka
set KOKA_UNINSTALL=N
set KOKA_HELP=N
set KOKA_FORCE=N
set KOKA_DIST_SOURCE=
set KOKA_DIST_URL=
set KOKA_DIST_BASE_URL=https://github.com/koka-lang/koka/releases/download
set KOKA_IEXPRESS=N
set KOKA_PREV_VERSION=
set KOKA_PREV_PREFIX=
set KOKA_ARCH=x64
set KOKA_VSCODE=N
set KOKA_DRYRUN=N

rem Use full path as under git bash the wrong "find" utility is found otherwise
set KOKA_FIND=%WINDIR%\System32\find.exe

rem On Windows for arm64, koka runs (for now) emulated as an x64 process (as ghc does not yet have a windows arm64 port)
rem Koka still generates native arm64 code though.
set KOKA_TARGET_ARCH=x64
for /F "tokens=1" %%x in ("%PROCESSOR_IDENTIFIER%") do (
  if "%%x" == "ARMv8" (set KOKA_TARGET_ARCH=arm64)
  if "%%x" == "ARMv9" (set KOKA_TARGET_ARCH=arm64)
  if "%%x" == "ARM64" (set KOKA_TARGET_ARCH=arm64)
)

set CLANG_PLATFORM=win64
if "%KOKA_TARGET_ARCH%" == "arm64" (set CLANG_PLATFORM=woa64)

set CLANG_REQUIRED_MAJOR=22
set CLANG_VERSION=22.1.1
set CLANG_INSTALL_BASE=LLVM-%CLANG_VERSION%-%CLANG_PLATFORM%.exe
set CLANG_INSTALL=%TEMP%\%CLANG_INSTALL_BASE%
set CLANG_INSTALL_URL=https://github.com/llvm/llvm-project/releases/download/llvmorg-%CLANG_VERSION%/%CLANG_INSTALL_BASE%
set CLANG_INSTALL_SHA256=e7fec06b4696be82bf8d4b846ebf6b810ed6f27d9eefdce46baf5330ea6e9ed4
if "%CLANG_PLATFORM%" == "woa64" (set CLANG_INSTALL_SHA256=a935deacb02611d5465b0d7aa2d30af06fb6862d614c50dc9cbf4f53983a90a6)

set VS_VERSION=2022
set VS_SDK_VERSION=Windows11SDK.26100
set VS_VCTOOLS_TARGET=x86.x64
if "%KOKA_TARGET_ARCH%" == "arm64" (set VS_VCTOOLS_TARGET=ARM64)
set VS_INSTALL_CMD=winget install Microsoft.VisualStudio.%VS_VERSION%.BuildTools --force --override "--wait --passive --add Microsoft.VisualStudio.Workload.VCTools --add Microsoft.VisualStudio.Component.VC.Tools.%VS_VCTOOLS_TARGET% --add Microsoft.VisualStudio.Component.%VS_SDK_VERSION%"


rem check if %LOCALAPPDATA% was not empty
if "%KOKA_PREFIX%" == "\koka" (set KOKA_PREFIX=c:\usr\local\koka)

rem process arguments
:args_parse
set kk_flag=%1

if "%kk_flag%" == "" goto args_done
if "%kk_flag:~0,1%" == "-" goto args_flag

set KOKA_DIST_SOURCE=%~1
goto args_next

:args_flag
  if "%kk_flag%" == "-u" (
    set KOKA_UNINSTALL=Y
    goto args_next
  )
  if "%kk_flag%" == "--uninstall" (
    set KOKA_UNINSTALL=Y
    goto args_next
  )
  if "%kk_flag%" == "-h" (
    set KOKA_HELP=Y
    goto args_next
  )
  if "%kk_flag%" == "--help" (
    set KOKA_HELP=Y
    goto args_next
  )
  if "%kk_flag%" == "-f" (
    set KOKA_FORCE=Y
    goto args_next
  )
  if "%kk_flag%" == "--force" (
    set KOKA_FORCE=Y
    goto args_next
  )
  if "%kk_flag%" == "--iexpress" (
    set KOKA_IEXPRESS=Y
    goto args_next
  )
  if "%kk_flag%" == "--vscode" (
    set KOKA_VSCODE=Y
    goto args_next
  )
  if "%kk_flag%" == "--dryrun" (
    set KOKA_DRYRUN=Y
    goto args_next
  )

  if "%kk_flag%" == "--version" (
    set KOKA_VERSION=%~2
    goto args_next2
  )
  if "%kk_flag%" == "-p" (
    set KOKA_PREFIX=%~2
    goto args_next2
  )
  if "%kk_flag%" == "--prefix"  (
    set KOKA_PREFIX=%~2
    goto args_next2
  )
  if "%kk_flag%" == "-b" (
    set KOKA_DIST_SOURCE=%~2
    goto args_next2
  )
  if "%kk_flag%" == "--bundle" (
    set KOKA_DIST_SOURCE=%~2
    goto args_next2
  )
  if "%kk_flag%" == "--url" (
    set KOKA_DIST_URL=%~2
    goto args_next2
  )

  echo unknown command line option: %kk_flag%
  set KOKA_HELP=Y
  goto args_next

:args_next2
shift
:args_next
shift
goto args_parse
:args_done

rem ---------------------------------------------------------
rem Defaults
rem ---------------------------------------------------------

if "%KOKA_VERSION:~0,1%" neq "v" set KOKA_VERSION=v%KOKA_VERSION%

if "%KOKA_VERSION%" leq "v2.1.6" set KOKA_ARCH=amd64

if "%KOKA_DIST_URL%" == "" (
  set KOKA_DIST_URL=%KOKA_DIST_BASE_URL%/%KOKA_VERSION%
)

if "%KOKA_DIST_SOURCE%" == "" (
  set KOKA_DIST_SOURCE=%KOKA_DIST_URL%/koka-%KOKA_VERSION%-windows-%KOKA_ARCH%.tar.gz
)

set KOKA_DIST_SOURCE_URL=
if ("%KOKA_DIST_SOURCE:~0,6%" == "ftp://") OR ("%KOKA_DIST_SOURCE:~0,7%" == "http://") OR ("%KOKA_DIST_SOURCE:~0,8%" == "https://") (
  set KOKA_DIST_SOURCE_URL=%KOKA_DIST_SOURCE%
  set KOKA_DIST_SOURCE=%TEMP%\koka-%KOKA_VERSION%-windows.tar.gz
)


rem ---------------------------------------------------------
rem Help & Uninstall
rem ---------------------------------------------------------

if "%KOKA_HELP%" == "Y"       goto help
if "%KOKA_UNINSTALL%" == "Y"  goto uninstall


rem ---------------------------------------------------------
rem Detect previous version
rem ---------------------------------------------------------

where /q koka
if errorlevel 1 goto prev_none
for /F "tokens=*" %%x in ('where koka 2^> nul ^| %KOKA_FIND% "\bin\koka.exe"') do (set KOKA_PREV_PREFIX=%%x)
if "%KOKA_PREV_PREFIX%" == "" goto prev_none
set KOKA_PREV_PREFIX=%KOKA_PREV_PREFIX:\bin\koka.exe=%
for /F "tokens=*" %%x in ('koka --version 2^> nul ^| %KOKA_FIND% "version: "') do (set KOKA_PREV_VERSION=%%x)
if "%KOKA_PREV_VERSION%" neq "" (set KOKA_PREV_VERSION=v%KOKA_PREV_VERSION:version: =%)
echo Found previous version: %KOKA_PREV_VERSION% at %KOKA_PREV_PREFIX%
:prev_none


rem ---------------------------------------------------------
rem Start install
rem ---------------------------------------------------------

if "%KOKA_DIST_SOURCE_URL%" == "" goto install_unpack
goto install_download


rem ---------------------------------------------------------
rem Help
rem ---------------------------------------------------------
:help

echo command:
echo   install-koka.bat [options] [bundle file/url]
echo.
echo options:
echo   -f, --force              continue without prompting
echo   -u, --uninstall          uninstall koka (%KOKA_VERSION%)
echo   -p, --prefix=^<dir^>       prefix directory (%KOKA_PREFIX%)
echo   --dryrun                 do not actually (un)install files
echo   --url=^<url^>              download base url (%KOKA_DIST_URL%)
echo   --version=^<ver^>          version tag (%KOKA_VERSION%)
echo.
goto end


rem ---------------------------------------------------------
rem Uninstall
rem ---------------------------------------------------------

:uninstall
echo Uninstalling %KOKA_VERSION% from prefix: %KOKA_PREFIX%

if not exist "%KOKA_PREFIX%\share\koka\%KOKA_VERSION%" (
  echo Cannot find koka version %KOKA_VERSION% at %KOKA_PREFIX%
  echo Done.
  goto end
)

set KOKA_ANSWER=N
if "%KOKA_FORCE%" neq "Y" (
  set /p "KOKA_ANSWER=Are you sure? [yN] "
)
if /i "%KOKA_ANSWER:~,1%" neq "Y" goto end

if exist "%KOKA_PREFIX%\bin\koka-%KOKA_VERSION%.exe" (
  echo - remove executable            : ^<prefix^>\bin\koka.exe
  fc /LB1 "%KOKA_PREFIX%\bin\koka.exe" "%KOKA_PREFIX%\bin\koka-%KOKA_VERSION%.exe" > nul 2> nul
  if not errorlevel 1 (del /Q "%KOKA_PREFIX%\bin\koka.exe")
  echo - remove executable            : ^<prefix^>\bin\koka-%KOKA_VERSION%.exe
  del /Q "%KOKA_PREFIX%\bin\koka-%KOKA_VERSION%.exe"
)
echo - remove pre-compiled libraries: ^<prefix^>\lib\koka\%KOKA_VERSION%
rmdir /S /Q "%KOKA_PREFIX%\lib\koka\%KOKA_VERSION%"
echo - remove source libraries      : ^<prefix^>\share\koka\%KOKA_VERSION%
rmdir /S /Q "%KOKA_PREFIX%\share\koka\%KOKA_VERSION%"

echo Done.

goto end


rem ---------------------------------------------------------
rem Install: download
rem ---------------------------------------------------------

:install_download

echo Downloading: %KOKA_DIST_SOURCE_URL%
curl --proto =https --tlsv1.2 -f -L -o "%KOKA_DIST_SOURCE%"  "%KOKA_DIST_SOURCE_URL%"
if errorlevel 1 (
  echo "curl error: %ERRORLEVEL%"
  goto end
)


rem ---------------------------------------------------------
rem Install: unpack
rem ---------------------------------------------------------

:install_unpack
echo.
echo Installing koka %KOKA_VERSION% to prefix: %KOKA_PREFIX%

if "%KOKA_DRYRUN%" == "Y" (
  echo Skip install due to dryrun.
  goto done_preinstall
)

if not exist %KOKA_PREFIX% (
  mkdir "%KOKA_PREFIX%"
)

echo - unpacking..
tar -xzf "%KOKA_DIST_SOURCE%" -C "%KOKA_PREFIX%"
if errorlevel 1 (
  echo Unpacking error: %ERRORLEVEL%
  echo.
  echo Perhaps Koka is in use by VS Code or another process?
  echo Or perhaps you are running the install.bat from a non-standard cmd shell (like git bash)?
  rem echo Or perhaps uninstall a previous version manually first? Use:
  rem echo   curl -sSL -o %%tmp%%\install-koka.bat https://github.com/koka-lang/koka/releases/download/%KOKA_VERSION%/install.bat ^&^& %%tmp%%\install-koka.bat --uninstall
  goto end
)

echo - install pre-compiled libraries: ^<prefix^>\lib\koka\%KOKA_VERSION%
echo - install source libraries      : ^<prefix^>\share\koka\%KOKA_VERSION%
echo - install executable            : ^<prefix^>\bin\koka.exe
echo - install symlink               : ^<prefix^>\bin\koka-%KOKA_VERSION%.exe
copy /B /Y "%KOKA_PREFIX%\bin\koka.exe" "%KOKA_PREFIX%\bin\koka-%KOKA_VERSION%.exe" > nul


rem -----------------------------------------------------------------
rem Install: set PATH environment variable.
rem Note: we need powershell to set the path globally as
rem the `setx` command cuts of environment values at 1024 characters!
rem -----------------------------------------------------------------

echo "%PATH%" | %KOKA_FIND% "%KOKA_PREFIX%\bin" >nul
if not errorlevel 1 goto done_env

rem Prevent duplicate semicolon
set KOKA_SEMI=;
if "%PATH:~-1%"==";" (set KOKA_SEMI=)
set "PATH=%PATH%%KOKA_SEMI%%KOKA_PREFIX%\bin"

where /q powershell
if not errorlevel 1 (
  echo.
  set KOKA_ANSWER=Y
  if "%KOKA_FORCE%" neq "Y" (
    set /p "KOKA_ANSWER=Add the koka binary directory to the search PATH? [Yn] "
  )
  if /i "%KOKA_ANSWER:~,1%" == "N" goto done_env

  echo - add binary directory to the user PATH environment variable.
  powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "[Environment]::SetEnvironmentVariable('PATH',\""$([Environment]::GetEnvironmentVariable('PATH','User'))%KOKA_SEMI%%KOKA_PREFIX%\bin\"",'User');"
  if not errorlevel 1 goto done_env
)
echo.
echo Please add "%KOKA_PREFIX%\bin" to your PATH environment variable.
echo.

:done_env

rem ---------------------------------------------------------
rem Editor support
rem ---------------------------------------------------------

if exist "%USERPROFILE%\.atom\packages" (
  echo - install atom editor support
  if not exist "%USERPROFILE%\.atom\packages\language-koka" (
    mkdir "%USERPROFILE%\.atom\packages\language-koka"
  )
  xcopy /Y /Q /S "%KOKA_PREFIX%\share\koka\%KOKA_VERSION%\contrib\atom\*" "%USERPROFILE%\.atom\packages\language-koka" > nul
  set  koka_editor=atom %%f:%%l:%%c
  setx koka_editor "atom %%f:%%l:%%c" > nul
)

rem Do not try to install an extension when invoked from vscode
if "%KOKA_VSCODE%" == "Y" goto done_vscode
where /Q code
if errorlevel 1 goto done_vscode

echo - install vscode editor support
code --list-extensions | %KOKA_FIND% "koka-lang.language-koka" > nul
if not errorlevel 1 (
  echo uninstall vscode ext
  code --uninstall-extension koka-lang.language-koka > nul
)
cmd /C "code --force --install-extension koka.language-koka"
if errorlevel 1 (
  echo Could not install VS Code editor support
  goto done_vscode
)

set  koka_editor=code --goto %%f:%%l:%%c
setx koka_editor "code --goto %%f:%%l:%%c" > nul

:done_vscode

where /Q emacs
if errorlevel 1 goto done_emacs
echo - emacs syntax mode installed at: %KOKA_PREFIX%\share\koka\%KOKA_VERSION%\contrib\emacs

:done_emacs
:done_preinstall

rem ---------------------------------------------------------
rem Uninstall previous version
rem ---------------------------------------------------------

if "%KOKA_PREV_PREFIX%" == "" goto done_install

rem always delete a previous koka.exe _if installed at a different prefix_ on the PATH
rem (so the newly installed koka gets found instead of an older one)
if "%KOKA_PREV_PREFIX%" neq "%KOKA_PREFIX%" (
  echo "%PATH%" | %KOKA_FIND% "%KOKA_PREV_PREFIX%\bin" >nul
  if not errorlevel 1 (
    if exist "%KOKA_PREV_PREFIX%\bin\koka.exe" (
      del /Q "%KOKA_PREV_PREFIX%\bin\koka.exe"
    )
  )
)

rem Did we update in place?
if "%KOKA_PREV_PREFIX%,%KOKA_PREV_VERSION%" == "%KOKA_PREFIX%,%KOKA_VERSION%" (
  echo Updated koka version %KOKA_VERSION% in-place
  goto done_install
)


if not exist "%KOKA_PREV_PREFIX%\lib\koka\%KOKA_PREV_VERSION%" goto done_install

if "%KOKA_DRYRUN%" == "Y" (
  echo Skip uninstall previous due to dryrun.
  goto done_install
)

echo.
set KOKA_ANSWER=N
if "%KOKA_FORCE%" neq "Y" (
  set /p "KOKA_ANSWER=Found previous koka installation %KOKA_PREV_VERSION%, Uninstall? [yN] "
)
if /i "%KOKA_ANSWER:~,1%" neq "Y" goto done_install

:uninstallprev
echo Uninstalling previous koka installation %KOKA_PREV_VERSION%..
if exist "%KOKA_PREV_PREFIX%\bin\koka-%KOKA_PREV_VERSION%.exe" (
  echo - remove executable            : ^<prefix^>\bin\koka-%KOKA_PREV_VERSION%.exe
  del /Q "%KOKA_PREV_PREFIX%\bin\koka-%KOKA_PREV_VERSION%.exe"
)
echo - remove pre-compiled libraries: ^<prefix^>\lib\koka\%KOKA_PREV_VERSION%
rmdir /S /Q "%KOKA_PREV_PREFIX%\lib\koka\%KOKA_PREV_VERSION%"
echo - remove source libraries      : ^<prefix^>\share\koka\%KOKA_PREV_VERSION%
rmdir /S /Q "%KOKA_PREV_PREFIX%\share\koka\%KOKA_PREV_VERSION%"



rem ---------------------------------------------------------
rem Install completed
rem ---------------------------------------------------------
:done_install


rem ---------------------------------------------------------
rem Install clang if needed
rem ---------------------------------------------------------

set CLANG_INSTALLED_VERSION="0"
set CLANG_INSTALLED_MAJOR="0"
set CLANG_EXE=clang-cl

where /q %CLANG_EXE%
if not errorlevel 1 goto clang_found

set CLANG_EXE=C:\Progra~1\LLVM\bin\clang-cl.exe
if exist %CLANG_EXE% goto clang_found
goto clang_notfound

:clang_found
for /F "tokens=3" %%x in ('%CLANG_EXE% --version ^| %KOKA_FIND% "clang version "') do (
   set CLANG_INSTALLED_VERSION=%%x
)
for /F "tokens=1 delims=." %%x in ("%CLANG_INSTALLED_VERSION%") do (
   set CLANG_INSTALLED_MAJOR=%%x
)

if %CLANG_INSTALLED_MAJOR% geq %CLANG_REQUIRED_MAJOR% (
  echo Found %CLANG_EXE% compiler version %CLANG_INSTALLED_VERSION%.
  goto clang_done
)

echo.
echo -----------------------------------------------------------------------
echo Found clang compiler version %CLANG_INSTALLED_VERSION%.
echo It is recommended to use at least version %CLANG_REQUIRED_MAJOR% for Koka.
goto clang_install

:clang_notfound

echo.
echo -----------------------------------------------------------------------
echo Cannot find the clang-cl compiler.
echo A C compiler is required for Koka to function.

if "%KOKA_DRYRUN%" == "Y" (
  echo Skip clang install due to dryrun.
  goto clang_done
)

:clang_install

set KOKA_ANSWER=Y
if "%KOKA_FORCE%" neq "Y" (
  set /p "KOKA_ANSWER=Would you like to download and install clang %CLANG_VERSION% for Windows? [Yn] "
)
if /i "%KOKA_ANSWER:~,1%" neq "Y" (
  echo Canceled automatic install.
  echo.
  goto clang_showurl
)

echo.
echo Downloading clang over https from:
echo  %CLANG_INSTALL_URL%
curl --proto =https --tlsv1.2 -f -L -o "%CLANG_INSTALL%" "%CLANG_INSTALL_URL%"
if errorlevel 1 goto clang_showurl

if "%CLANG_INSTALL_SHA256%" neq "" (
  echo Verifying sha256 hash ...
  timeout /T 1 > nul
  CertUtil -hashfile "%CLANG_INSTALL%" sha256 | %KOKA_FIND% "%CLANG_INSTALL_SHA256%" > nul
  if errorlevel 1 (
    echo Installation of %CLANG_INSTALL% is canceled as it does not match the
    echo expected sha256 signature: %CLANG_INSTALL_SHA256%
    echo.
    goto clang_showurl
  )
  echo Ok.
  timeout /T 1 > nul
)

echo.
echo Installing clang ...   (%CLANG_INSTALL%)
"%CLANG_INSTALL%"
if not errorlevel 1 (
  set "PATH=%PATH%;C:\Program Files\LLVM\bin"
)
del /Q "%CLANG_INSTALL%"
goto clang_done

:clang_showurl
echo Please install clang for Windows manually from:
echo   https://github.com/llvm/llvm-project/releases/latest  (Use 'LLVM-<version>-win64.exe')

:clang_done

rem ---------------------------------------------------------
rem Install Visual Studio Build tools if needed
rem ---------------------------------------------------------

if exist "C:\Program Files (x86)\Microsoft Visual Studio\2019" (
  echo Found Microsoft Visual Studio 2019 build tools.
  goto vs_done
)
if exist "C:\Program Files\Microsoft Visual Studio" (
  echo Found Microsoft Visual Studio build tools.
  goto vs_done
)

echo.
echo -----------------------------------------------------------------------
echo Cannot find the Windows Visual Studio build tools.
echo The build tools are required for Koka to compile to native code on Windows.

if "%KOKA_DRYRUN%" == "Y" (
  echo Skip vs build tools install due to dryrun.
  goto vs_done
)

where /q winget
if errorlevel 1 goto vs_showurl

set KOKA_ANSWER=Y
if "%KOKA_FORCE%" neq "Y" (
  set /p "KOKA_ANSWER=Would you like to download and install the Microsoft Visual Studio %VS_VERSION% build tools for Windows? [Yn] "
)
if /i "%KOKA_ANSWER:~,1%" neq "Y" (
  echo Canceled build tools install.
  echo.
  goto vs_showurl
)

echo Installing Microsoft Visual Studio %VS_VERSION% build tools:
echo   %VS_INSTALL_CMD%
%VS_INSTALL_CMD%
if errorlevel 1 (
  echo Could not install the Microsoft Visual Studio %VS_VERSION% build tools, error %ERRORLEVEL%.
)

goto vs_done

:vs_showurl
echo Please install Microsoft Visual Studio manually from:
echo   https://visualstudio.microsoft.com/downloads   (Install at least the C/C++ compiler)
echo.
echo Or try using 'winget' from the command prompt as:
echo   %VS_INSTALL_CMD%
echo.

:vs_done

rem ---------------------------------------------------------
rem End
rem ---------------------------------------------------------

echo.
echo -----------------------------------------------------------------------
echo Installed koka %KOKA_VERSION% to: %KOKA_PREFIX%\bin\koka
echo.

if "%KOKA_IEXPRESS%" == "Y" (
  set /p "KOKA_ANSWER=Press <enter> to finish installation.."
) else (
  if "%KOKA_VSCODE%" == "N" (
    echo Type 'koka' to enter the interactive compiler.
  )
)
echo.

rem This ends the local environment but still sets the given environment variables
endlocal & (
  set "koka_editor=%koka_editor%"
  set "PATH=%PATH%"
)
setlocal

:end
if "%KOKA_IEXPRESS%" == "Y" (
  set /p "KOKA_ANSWER=Press <enter> to finish installation.."
)
echo.
endlocal

