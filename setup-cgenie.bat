@ECHO OFF
IF EXIST "%USERPROFILE%\.cgenierc" (
  ECHO GENIE already set up -- if you want to start over, remove ~/.cgenierc
  EXIT /B 0
)
CALL tools\find_python.bat
IF NOT DEFINED PYTHON EXIT /B 0
%PYTHON% tools\setup-cgenie.py %*
