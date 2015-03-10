@ECHO OFF
IF NOT EXIST "%USERPROFILE%\.cgenierc" (
  ECHO GENIE not set up: run the setup-cgenie script^^!
  EXIT /B 0
)
SET _find_cmd=FINDSTR cgenie_root "%USERPROFILE%\.cgenierc"
FOR /F "tokens=2" %%r IN ('%_find_cmd%') DO (
  python %%r\tools\new-job.py %*
  EXIT /B 0
)
