@ECHO OFF
IF EXIST "%USERPROFILE%\.ctoasterrc" (
  ECHO cTOASTER already set up -- if you want to start over, remove ~/.ctoasterrc
  EXIT /B 0
)
CALL tools\find_python.bat
IF NOT DEFINED PYTHON EXIT /B 0
%PYTHON% tools\setup-ctoaster.py %*
