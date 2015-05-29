@ECHO OFF
IF NOT EXIST "%USERPROFILE%\.cgenierc" (
  ECHO GENIE not set up: run the setup-cgenie script^^!
  EXIT /B 0
)

:: Check for ANACONDA_DIR variable setting.
SET PYTHON="%ANACONDA_DIR%\bin\python.exe"
IF DEFINED ANACONDA_DIR GOTO:eof

ECHO ANACONDA_DIR environment variable not set^^!
ECHO
ECHO GENIE relies on the Anaconda Python installation.
ECHO You can find this at https://store.continuum.io/cshop/anaconda/
ECHO
ECHO Please install Anaconda and set the ANACONDA_DIR environment
ECHO variable to point to the Anaconda installation directory.
SET PYTHON=
EXIT /B 0
