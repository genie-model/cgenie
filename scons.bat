@ECHO OFF
CALL tools\find_python.bat
IF NOT DEFINED PYTHON EXIT /B 0
%PYTHON% tools/scons/scons.py %*
