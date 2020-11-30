@echo off

set _KOKA_VERSION=v2.0.12
set _KOKA_PREFIX=%APPDATA%\local
set _KOKA_DIST_SOURCE=
set _KOKA_DIST_SOURCE_URL=
set _KOKA_UNINSTALL=

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


if "%koka_version%" == "" goto done
if "%koka_version%" == "%_KOKA_VERSION%" goto done
if not exist "%_KOKA_PREFIX%\share\koka\%koka_version%" goto done

echo.
set _koka_answer=N
set /p "_koka_answer=Found previous koka version %koka_version%, Uninstall? [yN] " 
if /i "%_koka_answer:~,1%" NEQ "Y" goto done

:uninstallprev
echo Uninstall older koka version %koka_version%..
if exist "%_KOKA_PREFIX%\bin\koka-%koka_version%.exe" (del /Q "%_KOKA_PREFIX%\bin\koka-%koka_version%.exe")
rmdir /S /Q "%_KOKA_PREFIX%\lib\koka\%koka_version%"
rmdir /S /Q "%_KOKA_PREFIX%\share\koka\%koka_version%"
goto done


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


:done
set  koka_version=%_KOKA_VERSION%
setx koka_version %_KOKA_VERSION% > null

echo.
echo -----------------------------------------------------------------------
echo Installed koka %_KOKA_VERSION% to: %_KOKA_PREFIX%\bin\koka

echo "%PATH%" | find "%_KOKA_PREFIX%\bin" >nul
if errorlevel 1 (
  set "PATH=%PATH%;%_KOKA_PREFIX%\bin"
  echo.
  echo *** Please add "%_KOKA_PREFIX\bin" to you PATH environment variable. ***
)

echo -----------------------------------------------------------------------
:end