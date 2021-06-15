@echo off
rem ------------------------------------------------------------------
rem Installation script for Koka; use -h to see command line options.
rem ------------------------------------------------------------------

setlocal
set koka_version=v2.1.7
set koka_prefix=%LOCALAPPDATA%\local
set koka_uninstall=N
set koka_help=N
set koka_force=N
set koka_dist_source=
set koka_dist_source_url=
set koka_iexpress=N
set koka_prev_version=
set koka_prev_prefix=
set koka_arch=x64

set koka_clang_version=12.0.0
set koka_clang_install_base=LLVM-%koka_clang_version%-win64.exe
set koka_clang_install=%TEMP%\%koka_clang_install_base%
set koka_clang_install_url=https://github.com/llvm/llvm-project/releases/download/llvmorg-%koka_clang_version%/%koka_clang_install_base%
set koka_clang_install_sha256=8426d57f2af2bf07f80014bfd359e87ed10f5521a236a10cfe9fc4870d1b1b25

rem check if %LOCALAPPDATA% was not empty
if "%koka_prefix%" == "\local" (set koka_prefix=c:\usr\local)

rem process arguments
:argparse
if "%~1" == "" goto done_args
  if "%~1" == "-u" (
    set koka_uninstall=Y
    goto boolflag
  )
  if "%~1" == "--uninstall" (
    set koka_uninstall=Y
    goto boolflag
  )
  if "%~1" == "-h" (
    set koka_help=Y
    goto boolflag
  )
  if "%~1" == "--help" (
    set koka_help=Y
    goto boolflag
  )
  if "%~1" == "-f" (
    set koka_force=Y
    goto boolflag
  )
  if "%~1" == "--force" (
    set koka_force=Y
    goto boolflag
  )
  if "%~1" == "--iexpress" (
    set koka_iexpress=Y
    goto boolflag
  )
  if "%~1" == "-v"        (set koka_version=%~2)
  if "%~1" == "--version" (set koka_version=%~2)
  if "%~1" == "-p"        (set koka_prefix=%~2)
  if "%~1" == "--prefix"  (set koka_prefix=%~2)
  if "%~1" == "-b"        (set koka_dist_source=%~2)
  if "%~1" == "--bundle"  (set koka_dist_source=%~2)
  if "%~1" == "--url"     (set koka_dist_source_url=%~2)  
shift
:boolflag
shift
goto argparse
:done_args 

rem ---------------------------------------------------------
rem Defaults
rem ---------------------------------------------------------

if "%koka_version%" leq "v2.1.6" (set koka_arch=amd64)

if "%koka_dist_source_url%" == "" (
  set koka_dist_source_url=https://github.com/koka-lang/koka/releases/download/%koka_version%/koka-%koka_version%-windows-%koka_arch%.tar.gz
)


rem ---------------------------------------------------------
rem Help & Uninstall
rem ---------------------------------------------------------

if "%koka_help%" == "Y"       goto help
if "%koka_uninstall%" == "Y"  goto uninstall

rem ---------------------------------------------------------
rem Detect previous version
rem ---------------------------------------------------------

where /q koka
if errorlevel 1 goto prev_none
for /F "tokens=*" %%x in ('where koka 2^> nul ^| find "\bin\koka.exe"') do (set koka_prev_prefix=%%x)
if "%koka_prev_prefix%" == "" goto prev_none
set koka_prev_prefix=%koka_prev_prefix:\bin\koka.exe=%
for /F "tokens=*" %%x in ('koka --version 2^> nul ^| find "version: "') do (set koka_prev_version=%%x)
if "%koka_prev_version%" neq "" (set koka_prev_version=v%koka_prev_version:version: =%)
:prev_none

rem ---------------------------------------------------------
rem Start install
rem ---------------------------------------------------------

if "%koka_dist_source%" == "" goto install_download 
goto install_unpack


rem ---------------------------------------------------------
rem Help
rem ---------------------------------------------------------
:help

echo command:
echo   install-koka.bat [options]
echo.
echo options:
echo   -f, --force              continue without prompting
echo   -u, --uninstall          uninstall koka (%koka_version%)
echo   -p, --prefix=^<dir^>       prefix directory (%koka_prefix%)
echo   -b, --bundle=^<file^|url^>  full bundle location (%koka_dist_source%)
echo   --url=^<url^>              download url (%koka_dist_source_url%)
echo   --version=^<ver^>          version tag (%koka_version%)
echo.
goto end


rem ---------------------------------------------------------
rem Uninstall
rem ---------------------------------------------------------

:uninstall
echo Uninstalling %koka_version% from prefix: %koka_prefix%

if not exist "%koka_prefix%\share\koka\%koka_version%" (
  echo Cannot find koka version %koka_version% at %koka_prefix%
  echo Done. 
  goto end
)

set koka_answer=N
if "%koka_force%" neq "Y" (
  set /p "koka_answer=Are you sure? [yN] " 
)
if /i "%koka_answer:~,1%" neq "Y" goto end

if exist "%koka_prefix%\bin\koka-%koka_version%.exe" (
  echo - remove executable            : ^<prefix^>\bin\koka.exe
  fc /LB1 "%koka_prefix%\bin\koka.exe" "%koka_prefix%\bin\koka-%koka_version%.exe" > nul 2> nul
  if not errorlevel 1 (del /Q "%koka_prefix%\bin\koka.exe")
  echo - remove executable            : ^<prefix^>\bin\koka-%koka_version%.exe
  del /Q "%koka_prefix%\bin\koka-%koka_version%.exe"
)
echo - remove pre-compiled libraries: ^<prefix^>\lib\koka\%koka_version%
rmdir /S /Q "%koka_prefix%\lib\koka\%koka_version%"
echo - remove source libraries      : ^<prefix^>\share\koka\%koka_version%
rmdir /S /Q "%koka_prefix%\share\koka\%koka_version%"

echo Done.

goto end


rem ---------------------------------------------------------
rem Install: download
rem ---------------------------------------------------------

:install_download

set koka_dist_source=%TEMP%\koka-%koka_version%-windows.tar.gz
  
echo Downloading: %koka_dist_source_url%
curl --proto =https --tlsv1.2 -f -L -o "%koka_dist_source%"  "%koka_dist_source_url%"
if errorlevel 1 (
  echo "curl error: %ERRORLEVEL%"
  goto end
)

rem ---------------------------------------------------------
rem Install: unpack 
rem ---------------------------------------------------------

:install_unpack
echo.
echo Installing to prefix: %koka_prefix%
if not exist %koka_prefix% (
  mkdir "%koka_prefix%"
)

echo - unpacking..
tar -xzf "%koka_dist_source%" -C "%koka_prefix%"
if errorlevel 1 (
  echo "Unpacking error: %ERRORLEVEL%"
  goto end
)

echo - install pre-compiled libraries to: ^<prefix^>\lib\koka\%koka_version%
echo - install source libraries to      : ^<prefix^>\share\koka\%koka_version%
echo - install executable to            : ^<prefix^>\bin\koka.exe
echo - install symlink to               : ^<prefix^>\bin\koka-%koka_version%.exe
copy /B /Y "%koka_prefix%\bin\koka.exe" "%koka_prefix%\bin\koka-%koka_version%.exe" > nul


rem -----------------------------------------------------------------
rem Install: set PATH environment variable.
rem Note: we need powershell to set the path globally as 
rem the `setx` command cuts of environment values at 1024 characters!
rem -----------------------------------------------------------------

echo "%PATH%" | find "%koka_prefix%\bin" >nul
if not errorlevel 1 goto done_env

rem Prevent duplicate semicolon
set koka_semi=;
if "%PATH:~-1%"==";" (set koka_semi=)
set PATH=%PATH%%koka_semi%%koka_prefix%\bin

where /q powershell
if not errorlevel 1 (
  echo.
  set koka_answer=Y
  if "%koka_force%" neq "Y" (
    set /p "koka_answer=Add the koka binary directory to the search PATH? [Yn] " 
  )
  if /i "%koka_answer:~,1%" == "N" goto done_env

  echo - add binary directory to the user PATH environment variable.
  powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "[Environment]::SetEnvironmentVariable('PATH',\""$([Environment]::GetEnvironmentVariable('PATH','User'))%koka_semi%%koka_prefix%\bin\"",'User');"
  if not errorlevel 1 goto done_env
)
echo.
echo Please add "%koka_prefix%\bin" to your PATH environment variable.
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
  xcopy /Y /Q /S "%koka_prefix%\share\koka\%koka_version%\contrib\atom\*" "%USERPROFILE%\.atom\packages\language-koka" > nul
  set  koka_editor=atom %%f:%%l:%%c
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

set  koka_editor=code --goto %%f:%%l:%%c
setx koka_editor "code --goto %%f:%%l:%%c" > nul

:done_vscode

where /Q emacs
if errorlevel 1 goto done_emacs
echo - emacs syntax mode can be found at: %koka_prefix%\share\koka\%koka_version%\contrib\emacs

:done_emacs


rem ---------------------------------------------------------
rem Uninstall previous version
rem ---------------------------------------------------------

if "%koka_prev_prefix%" == "" goto done_install

rem always delete a previous koka.exe _if installed at a different prefix_ on the PATH 
rem (so the newly installed koka gets found instead of an older one)
if "%koka_prev_prefix%" neq "%koka_prefix%" (
  echo "%PATH%" | find "%koka_prev_prefix%\bin" >nul
  if not errorlevel 1 (
    if exist "%koka_prev_prefix%\bin\koka.exe" (
      del /Q "%koka_prev_prefix%\bin\koka.exe"
    )
  )
)

rem Did we update in place?
if "%koka_prev_prefix%,%koka_prev_version%" == "%koka_prefix%,%koka_version%" (
  echo Updated koka version %koka_version% in-place
  goto done_install
)


if not exist "%koka_prev_prefix%\lib\koka\%koka_prev_version%" goto done_install

echo.
set koka_answer=N
if "%koka_force%" neq "Y" (
  set /p "koka_answer=Found previous koka installation %koka_prev_version%, Uninstall? [yN] " 
)
if /i "%koka_answer:~,1%" neq "Y" goto done_install

:uninstallprev
echo Uninstalling previous koka installation %koka_prev_version%..
if exist "%koka_prev_prefix%\bin\koka-%koka_prev_version%.exe" (
  echo - remove executable            : ^<prefix^>\bin\koka-%koka_prev_version%.exe  
  del /Q "%koka_prev_prefix%\bin\koka-%koka_prev_version%.exe"
)
echo - remove pre-compiled libraries: ^<prefix^>\lib\koka\%koka_prev_version%
rmdir /S /Q "%koka_prev_prefix%\lib\koka\%koka_prev_version%"
echo - remove source libraries      : ^<prefix^>\share\koka\%koka_prev_version%
rmdir /S /Q "%koka_prev_prefix%\share\koka\%koka_prev_version%"



rem ---------------------------------------------------------
rem Install completed
rem ---------------------------------------------------------
:done_install


rem ---------------------------------------------------------
rem Install clang if needed
rem ---------------------------------------------------------

where /q clang-cl
if not errorlevel 1 goto done_clang

echo.
echo -----------------------------------------------------------------------
echo Cannot find the clang-cl compiler. 
echo A C compiler is required for Koka to function.

set koka_answer=Y
if "%koka_force%" neq "Y" (
  set /p "koka_answer=Would you like to download and install clang %koka_clang_version% for Windows? [Yn] " 
)
if /i "%koka_answer:~,1%" neq "Y" (
  echo Canceled automatic install.
  echo.
  goto koka_clang_showurl
)

echo.
echo Downloading clang over https from: 
echo  %koka_clang_install_url%
curl --proto =https --tlsv1.2 -f -L -o "%koka_clang_install%" "%koka_clang_install_url%"
if errorlevel 1 goto koka_clang_showurl

if "%koka_clang_install_sha256%" neq "" (
  echo Verifying sha256 hash ...
  timeout /T 1 > nul  
  CertUtil -hashfile "%koka_clang_install%" sha256 | find "%koka_clang_install_sha256%" > nul
  if errorlevel 1 (
    echo Installation of %koka_clang_install% is canceled as it does not match the
    echo expected sha256 signature: %koka_clang_install_sha256%
    echo.
    goto koka_clang_showurl
  )
  echo Ok.
  timeout /T 1 > nul  
)

echo.
echo Installing clang ...   (%koka_clang_install%)
"%koka_clang_install%"
if not errorlevel 1 (
  set PATH=%PATH%;C:\Program Files\LLVM\bin
)
del /Q "%koka_clang_install%"
goto done_clang

:koka_clang_showurl
echo Please install clang for Windows manually from: https://llvm.org/builds

:done_clang

rem ---------------------------------------------------------
rem End
rem ---------------------------------------------------------

echo.
echo -----------------------------------------------------------------------
echo Installed koka %koka_version% to: %koka_prefix%\bin\koka
echo.

if "%koka_iexpress%" == "Y" (
  set /p "koka_answer=Press <enter> to finish installation.." 
) else (
  echo Type 'koka' to enter the interactive compiler.
)
echo.

rem This ends the local environment but still sets the given environment variables
endlocal & (
  set koka_editor=%koka_editor%
  set PATH=%PATH%
)
setlocal

:end
if "%koka_iexpress%" == "Y" (
  set /p "koka_answer=Press <enter> to finish installation.." 
)
echo.
endlocal

