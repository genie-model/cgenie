@ECHO OFF
IF NOT EXIST "%USERPROFILE%\.ctoasterrc" (
  ECHO cTOASTER not set up: run the setup-ctoaster script^^!
  EXIT /B 0
)
SET _find_cmd=FINDSTR ctoaster_root "%USERPROFILE%\.ctoasterrc"
FOR /F "tokens=2" %%r IN ('%_find_cmd%') DO (
  CALL %%r\tools\find_python.bat
  IF NOT DEFINED PYTHON EXIT /B 0
  %PYTHON% %%r\tools\ctoaster-gui.py %*
  EXIT /B 0
)
