@echo off

set _KOKA_VERSION=v2.1.3
set _KOKA_PREFIX=%APPDATA%\local
set _KOKA_DIST_SOURCE=
set _KOKA_DIST_SOURCE_URL=
set _KOKA_UNINSTALL=

set _CLANG_VERSION=11.0.0
set _CLANG_INSTALL=LLVM-%_CLANG_VERSION%-win64.exe
set _CLANG_INSTALL_URL=https://github.com/llvm/llvm-project/releases/download/llvmorg-%_CLANG_VERSION%/%_CLANG_INSTALL%
set _CLANG_INSTALL_SHA256=a773ee3519ecc8d68d91f0ec72ee939cbed8ded483ba8e10899dc19bccba1e22

:cmds
if "%~1" == "" goto cont
  if "%~1" == "-u"        (
    set _KOKA_UNINSTALL=Y
    goto boolflag
  )
  if "%~1" == "--uninstall"        (
    set _KOKA_UNINSTALL=Y
    goto boolflag
  )
  if "%~1" == "-v"        (set _KOKA_VERSION=%2)
  if "%~1" == "--version" (set _KOKA_VERSION=%2)
  if "%~1" == "-p"        (set _KOKA_PREFIX=%2)
  if "%~1" == "--prefix"  (set _KOKA_PREFIX=%2)
  if "%~1" == "-b"        (set _KOKA_DIST_SOURCE=%2)
  if "%~1" == "--bundle"  (set _KOKA_DIST_SOURCE=%2)
  if "%~1" == "--url"     (set _KOKA_DIST_SOURCE_URL=%2)
shift
:boolflag
shift
goto cmds
:cont

if "%_KOKA_UNINSTALL%" == "Y" goto uninstall


if not "%_KOKA_DIST_SOURCE%" == "" goto unpack

set _KOKA_DIST_SOURCE=%TEMP%\koka-dist.tar.gz
if "%_KOKA_DIST_SOURCE_URL%" == "" (
  set _KOKA_DIST_SOURCE_URL=https://github.com/koka-lang/koka/releases/download/%_KOKA_VERSION%/koka-%_KOKA_VERSION%-windows-amd64.tar.gz
)

echo Downloading koka %_KOKA_VERSION% binary distribution..
echo   %_KOKA_DIST_SOURCE_URL%
curl -f -L -o %_KOKA_DIST_SOURCE%  %_KOKA_DIST_SOURCE_URL%
if errorlevel 1 (
  echo "curl error: %ERRORLEVEL%"
  goto:eof
)

:unpack
echo.
echo Installing to   : %_KOKA_PREFIX%
if not exist %_KOKA_PREFIX% (
  mkdir %_KOKA_PREFIX%
)

echo Unpacking       : %_KOKA_DIST_SOURCE%
tar -xzf %_KOKA_DIST_SOURCE% -C %_KOKA_PREFIX%
if errorlevel 1 (
  echo "tar unpacking error: %ERRORLEVEL%"
  goto:eof
)

if exist "%USERPROFILE%\.atom\packages" (
  echo Install Atom editor support..
  if not exist "%USERPROFILE%\.atom\packages\language-koka" (
    mkdir "%USERPROFILE%\.atom\packages\language-koka"
  )
  xcopy /Y /Q /S "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%\contrib\atom\*" "%USERPROFILE%\.atom\packages\language-koka"
  set  "koka_editor=atom %%f:%%l:%%c"
  setx koka_editor "atom %%f:%%l:%%c" > nul
)

if exist "%USERPROFILE%\.vscode\extensions" (
  echo Install VS Code editor support..
  xcopy /Y /Q /S "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%\contrib\vscode\*" "%USERPROFILE%\.vscode\extensions"
  set  "koka_editor=code --goto %%f:%%l:%%c"
  setx koka_editor "code --goto %%f:%%l:%%c" > nul
)


if "%koka_version%" == "" goto doneinstall
if "%koka_version%" == "%_KOKA_VERSION%" (
  echo Updated koka version %_KOKA_VERSION% in-place
  goto doneinstall
)
if not exist "%_KOKA_PREFIX%\share\koka\%koka_version%" goto doneinstall

echo.
set _koka_answer=N
set /p "_koka_answer=Found previous koka version %koka_version%, Uninstall? [yN] " 
if /i "%_koka_answer:~,1%" NEQ "Y" goto doneinstall

:uninstallprev
echo Uninstall older koka version %koka_version%..
if exist "%_KOKA_PREFIX%\bin\koka-%koka_version%.exe" (del /Q "%_KOKA_PREFIX%\bin\koka-%koka_version%.exe")
rmdir /S /Q "%_KOKA_PREFIX%\lib\koka\%koka_version%"
rmdir /S /Q "%_KOKA_PREFIX%\share\koka\%koka_version%"
goto doneinstall


:uninstall
echo Uninstall koka version %_KOKA_VERSION%

if not exist "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%" (
  echo Cannot find koka version %_KOKA_VERSION% at %_KOKA_PREFIX%
  echo Done. 
  goto end
)

echo.
set _koka_answer=N
set /p "_koka_answer=Removing koka version %_KOKA_VERSION%, Are you sure? [yN] " 
if /i "%_koka_answer:~,1%" NEQ "Y" goto end

echo Uninstalling..
if exist "%_KOKA_PREFIX%\bin\koka-%_KOKA_VERSION%.exe" (del /Q "%_KOKA_PREFIX%\bin\koka-%_KOKA_VERSION%.exe")
rmdir /S /Q "%_KOKA_PREFIX%\lib\koka\%_KOKA_VERSION%"
rmdir /S /Q "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%"
echo Done.
goto end


:doneinstall

where /q clang-cl
if errorlevel 1 goto clangaskinstall
goto done


:clangaskinstall
echo.
echo -----------------------------------------------------------------------
echo Cannot find the clang-cl compiler. 
echo A C compiler is required for Koka to function.

set _koka_answer=Y
set /p "_koka_answer=Would you like to download and install Clang %_CLANG_VERSION% for Windows? [Yn] " 
if /i "%_koka_answer:~,1%" NEQ "Y" (
  echo Canceled automatic install.
  echo.
  goto clangshowurl
)

echo.
echo Downloading Clang from: 
echo  %_CLANG_INSTALL_URL%
curl -f -L -o "%_CLANG_INSTALL%" "%_CLANG_INSTALL_URL%"
if errorlevel 1 goto clangshowurl

if "%_CLANG_INSTALL_SHA256%" NEQ "" (
  echo Verifying sha256 hash ...
  CertUtil -hashfile ".\%_CLANG_INSTALL%" sha256 | find "%_CLANG_INSTALL_SHA256%" > nul
  if errorlevel 1 (
    echo Installation of %_CLANG_INSTALL% is canceled as it does not match the
    echo expected sha256 signature: %_CLANG_INSTALL_SHA256%
    echo.
    goto clangshowurl
  )
  echo Ok.
)

echo.
echo Installing Clang ...   (.\%_CLANG_INSTALL%)
".\%_CLANG_INSTALL%"
goto done

:clangshowurl
echo Please install the Clang for Windows manually from: https://llvm.org/builds


:done
set  koka_version=%_KOKA_VERSION%
setx koka_version %_KOKA_VERSION% >nul

echo.
echo -----------------------------------------------------------------------
echo Installed koka %_KOKA_VERSION% to: %_KOKA_PREFIX%\bin\koka
echo.

echo "%PATH%" | find "%_KOKA_PREFIX%\bin" >nul
if errorlevel 1 (
  set "PATH=%PATH%;%_KOKA_PREFIX%\bin"
  where /q powershell
  if not errorlevel 1 (
    powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "[Environment]::SetEnvironmentVariable('PATH',\""$([Environment]::GetEnvironmentVariable('PATH','User'))\;%_KOKA_PREFIX%\bin\"",'User');"
    if not errorlevel 1 goto endmsg
  )
  echo Please add "%_KOKA_PREFIX%\bin" to you PATH environment variable.
  echo.
)

:endmsg
echo Type 'koka' to enter the interactive compiler.
echo.

:end

