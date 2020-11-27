@echo off

set _KOKA_VERSION=v2.0.9
set _KOKA_PREFIX=%APPDATA%\local
set _KOKA_DIST_SOURCE=
set _KOKA_DIST_SOURCE_URL=

:cmds
if "%~1" == "" goto cont
  if "%~1" == "-v"        (set _KOKA_VERSION=%2)
  if "%~1" == "--version" (set _KOKA_VERSION=%2)
  if "%~1" == "-p"        (set _KOKA_PREFIX=%2)
  if "%~1" == "--prefix"  (set _KOKA_PREFIX=%2)
  if "%~1" == "-b"        (set _KOKA_DIST_SOURCE=%2)
  if "%~1" == "--bundle"  (set _KOKA_DIST_SOURCE=%2)
  if "%~1" == "--url"     (set _KOKA_DIST_SOURCE_URL=%2)
shift
shift
goto cmds
:cont

if not "%_KOKA_DIST_SOURCE%" == "" goto unpack

set _KOKA_DIST_SOURCE=%TEMP%\koka-dist.tar.gz

if "%_KOKA_DIST_SOURCE_URL%" == "" (
  set _KOKA_DIST_SOURCE_URL=https://github.com/koka-lang/koka/releases/download/%_KOKA_VERSION%/koka-%_KOKA_VERSION%-win-x86_64.tar.gz
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

if exist "%HOMEDRIVE%%HOMEPATH%\.atom\packages" (
  echo Install Atom editor support..
  if not exist "%HOMEDRIVE%%HOMEPATH%\.atom\packages\language-koka" (
    mkdir "%HOMEDRIVE%%HOMEPATH%\.atom\packages\language-koka"
  )
  xcopy /Y /Q /S "%_KOKA_PREFIX%\share\koka\%_KOKA_VERSION%\contrib\atom\*" "%HOMEDRIVE%%HOMEPATH%\.atom\packages\language-koka"
)

echo -----------------------------------------------------------------------
echo Installed koka to: %_KOKA_PREFIX%\bin\koka

echo "%PATH%" | find "%_KOKA_PREFIX%\bin" >nul
if errorlevel 1 (
  rem not in PATH
  set "PATH=%PATH%;%_KOKA_PREFIX%\bin"
  echo.
  echo ** Please add "%_KOKA_PREFIX\bin" to you PATH environment variable. **
)

echo -----------------------------------------------------------------------
