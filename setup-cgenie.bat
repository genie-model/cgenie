@ECHO OFF
IF EXIST "%USERPROFILE%\.cgenierc" (
  ECHO GENIE already set up -- if you want to start over, remove ~/.cgenierc
  EXIT /B 0
)
python tools\setup-cgenie.py %*
