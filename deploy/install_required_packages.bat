@REM Use Rscript.exe to run install_required_packages.R
@echo off

set SCRIPT_NAME=install_required_packages.R
if [%1]==[] set SCRIPT_PATH="%CD%"\%SCRIPT_NAME%
if not [%1]==[] set SCRIPT_PATH=%1\%SCRIPT_NAME%

call :set_R_bin_dir
if [%R_DIR%]==[] exit /b 1

set R_SCRIPT=%R_DIR%\Rscript.exe
if not exist %R_SCRIPT% (echo Cannot find %R_SCRIPT% & pause & exit /b 1)

%R_SCRIPT% %SCRIPT_PATH%
pause
exit /b %ERRORLEVEL%

:set_R_bin_dir

SETLOCAL
set REGKEY=HKEY_LOCAL_MACHINE\SOFTWARE\R-core\R64
set BIN_SUFFIX=bin\x64
set REGVAL=InstallPath
echo Checking for R 64-bit
call :get_reg_key_value
if "%REGDATA%"=="" (
	echo Checking for R 32-bit
    set REGKEY=HKEY_LOCAL_MACHINE\SOFTWARE\R-core\R32
    set BIN_SUFFIX=bin\i386
    call :get_reg_key_value
)
if "%REGDATA%"=="" (echo Cannot find R directory. Is R installed? & pause & exit /b 1)  
ENDLOCAL & set R_DIR="%REGDATA%"\%BIN_SUFFIX%
echo R found in %R_DIR%
echo.

exit /b 0

:get_reg_key_value

::Lookup REGKEY/REGVAL is registry 

::Check for presence of key first.
reg query %REGKEY% /v %REGVAL% >nul 2>nul || (exit /b 1)

::query the value. pipe it through findstr in order to find the matching line that has the value. 
::only grab token 3 and the remainder of the line. %%b is what we are interested in here.
set REGDATA=
for /f "tokens=2,*" %%a in ('reg query %REGKEY% /v %REGVAL% ^| findstr %REGVAL%') do (
    set REGDATA=%%b
)

::Possibly no value set
if not defined REGDATA (exit /b 2)

exit /b 0