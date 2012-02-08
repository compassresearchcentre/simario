@REM Installs simar library using R CMD install. Automatically locates R via the registry.
@echo off
set SIMAR_FILE_NAME=simar*.tar.gz

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

call :set_R_dir
if [%R_DIR%]==[] exit /b 1

set R_EXE=%R_DIR%\bin\R.exe
if not exist %R_EXE% (echo Cannot find %R_EXE% & pause & exit /b 1)

for %%s in (%SIMAR_PATH%) do (
echo Installing %%s
%R_EXE% CMD INSTALL "%%s"
)

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