@echo off

set _KOKA_PREFIX=%APPDATA%\local
set _KOKA_VERSION=v2.0.8

if not "%~1" == "" ( set _KOKA_PREFIX=%1)
if not "%~2" == "" ( set _KOKA_VERSION=%2)

set _KOKA_DIST_SOURCE_URL=https://github.com/koka-lang/koka/releases/download/%_KOKA_VERSION%/koka-%_KOKA_VERSION%-win-x86_64.tar.gz

echo Downloading koka %_KOKA_VERSION% binary distribution..
echo   %_KOKA_DIST_SOURCE_URL%
curl -f -L -o %TEMP%\koka-dist.tar.gz %_KOKA_DIST_SOURCE_URL%
if errorlevel 1 (
  echo "curl error: %ERRORLEVEL%"
  goto:eof
)

echo.
echo Installing to   : %_KOKA_PREFIX%
if not exist %_KOKA_PREFIX% (
  mkdir %_KOKA_PREFIX%
)

echo Unpacking       : %TEMP%\koka-dist.tar.gz
tar -xzf %TEMP%\koka-dist.tar.gz -C %_KOKA_PREFIX%
if errorlevel 1 (
  echo "tar unpacking error: %ERRORLEVEL%"
  goto:eof
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
