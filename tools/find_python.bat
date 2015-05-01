@ECHO OFF
IF NOT EXIST "%USERPROFILE%\.cgenierc" (
  ECHO GENIE not set up: run the setup-cgenie script^^!
  EXIT /B 0
)

:: Check for GENIE_PYTHON variable setting.
SET PYTHON="%GENIE_PYTHON%"
IF DEFINED GENIE_PYTHON GOTO:eof

:: Check for suitable 'python' executable.
SET PYTHON=python
FOR /F "delims=" %%v IN ('python -V 2^>^&1') DO ( SET AVV=%%v )
IF "%AVV%" == "Python 2.7.9 " GOTO:eof

:: Check for suitable 'python' executable in C:\Python27 directory.
SET PYTHON="C:\Python27\python"
IF EXIST C:\Python27 GOTO:eof

ECHO NO SUITABLE PYTHON INSTALLATION FOUND^^!
SET PYTHON=
EXIT /B 0
