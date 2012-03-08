@REM Installs simar library using R CMD install. Automatically locates R via the registry.
@echo off
set SIMAR_FILE_NAME=simar*.tar.gz
@REM Clearing R_USER environment variable to prevent Windows 7 attempting to install to Program Files.
set R_USER=

goto START

:USAGE
echo.
echo Usage:
echo.
echo %0 simar_path
echo.
echo   simar_path         path to a folder containing a single file with the name  
echo                      %SIMAR_FILE_NAME%. Defaults to the current directory.
pause
exit /b 1

:START

if [%1]==[] set SIMAR_FOLDER="%CD%"
if not [%1]==[] set SIMAR_FOLDER=%1
set SIMAR_PATH=%SIMAR_FOLDER%\%SIMAR_FILE_NAME%

if not exist %SIMAR_PATH% (echo simar not found in %SIMAR_PATH% & goto USAGE)

call :set_R_bin_dir
if [%R_DIR%]==[] exit /b 1

set R_EXE=%R_DIR%\R.exe
if not exist %R_EXE% (echo Cannot find %R_EXE% & pause & exit /b 1)

for %%s in (%SIMAR_PATH%) do (
echo Installing %%s
%R_EXE% CMD INSTALL "%%s"
)

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