@ECHO OFF
SET _find_cmd=FINDSTR cgenie_root "%USERPROFILE%\.cgenierc"
FOR /F "tokens=2" %%r IN ('%_find_cmd%') DO (
  SET "PATH=%%r\tools\netcdf\%TARGET_ARCH%\lib;%PATH%"
  python %%r\tools\go.py %*
  EXIT /B 0
)
