@REM Use Rscript.exe to run install_required_packages.R
@echo off

set SCRIPT_NAME=install_required_packages.R
if [%1]==[] set SCRIPT_PATH="%CD%"\%SCRIPT_NAME%
if not [%1]==[] set SCRIPT_PATH=%1\%SCRIPT_NAME%


call :set_R_dir
if [%R_DIR%]==[] exit /b 1

set R_SCRIPT=%R_DIR%\bin\Rscript.exe
if not exist %R_SCRIPT% (echo Cannot find %R_SCRIPT% & pause & exit /b 1)

%R_SCRIPT% %SCRIPT_PATH%
pause
exit /b %ERRORLEVEL%

:set_R_dir

::Use registry to locate and set R_DIR to the R installation directory (eg: "C:\Program Files\R\R-2.14.1") 
set REGKEY_R=HKEY_LOCAL_MACHINE\SOFTWARE\R-core\R
set REGVAL_INSTALL_PATH=InstallPath

::Check for presence of key first.
reg query %REGKEY_R% /v %REGVAL_INSTALL_PATH% >nul 2>nul || (echo Missing registry key value %REGKEY_R%\%REGVAL_INSTALL_PATH%. Is R installed? & pause & exit /b 1)

::query the value. pipe it through findstr in order to find the matching line that has the value. only grab token 3 and the remainder of the line. %%b is what we are interested in here.
set R_DIR=
for /f "tokens=2,*" %%a in ('reg query %REGKEY_R% /v %REGVAL_INSTALL_PATH% ^| findstr %REGVAL_INSTALL_PATH%') do (
    set R_DIR=%%b
)

::Possibly no value set
if not defined R_DIR (echo No value set for %REGKEY_R%\%REGVAL_INSTALL_PATH%. & pause & exit /b 1)

set R_DIR="%R_DIR%"
echo R found in %R_DIR%
echo.

exit /b 0