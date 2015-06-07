@ECHO OFF

:: Check for CGENIE_PYTHON variable setting.
SET PYTHON="%CGENIE_PYTHON%"
IF DEFINED CGENIE_PYTHON GOTO:eof

ECHO(
ECHO NO SUITABLE PYTHON SETUP FOUND^^!
ECHO(
ECHO The CGENIE_PYTHON environment variable needs to be set to
ECHO point to the Python interpreter executable (which must be
ECHO Python 2.7.9).
ECHO(
ECHO We recommend that you install the Anaconda Python installation
ECHO into C:\Anaconda and set CGENIE_PYTHON to
ECHO C:\Anaconda\python.exe
ECHO(

SET PYTHON=
EXIT /B 0
