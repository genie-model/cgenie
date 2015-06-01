@ECHO OFF
IF NOT EXIST "%USERPROFILE%\.cgenierc" (
  ECHO GENIE not set up: run the setup-cgenie script^^!
  EXIT /B 0
)
SET _find_cmd=FINDSTR cgenie_root "%USERPROFILE%\.cgenierc"
FOR /F "tokens=2" %%r IN ('%_find_cmd%') DO (
  CALL %%r\tools\find_python.bat
  IF NOT DEFINED PYTHON EXIT /B 0
  %PYTHON% %%r\tools\cgenie-gui.py %*
  EXIT /B 0
)
