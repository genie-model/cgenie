@ECHO OFF

:: Check for CGENIE_PYTHON variable setting.
SET PYTHON="%CGENIE_PYTHON%"
IF DEFINED CGENIE_PYTHON GOTO:eof

:: Check for ANACONDA_DIR variable setting.
SET PYTHON="%ANACONDA_DIR%\bin\python.exe"
IF DEFINED ANACONDA_DIR GOTO:eof

ECHO NO SUITABLE PYTHON SETUP FOUND^^!
ECHO
ECHO You have two choices:
ECHO
ECHO 1. Set the CGENIE_PYTHON environment variable to point to a
ECHO    Python 2.7.9 interpreter that has the matplotlib package
ECHO    installed.
ECHO
ECHO 2. Install the Anaconda Python installation and set the ANACONDA_DIR
ECHO    environment variable to point to the installation directory.
ECHO
ECHO Of these two options, the second is preferred for Windows installations,
ECHO because it's simpler to manage.

SET PYTHON=
EXIT /B 0
