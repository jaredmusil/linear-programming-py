@echo off

set python=%CD%\exe\portable-python\App\pythonw.exe
set app=%CD%\app.py

::echo %python%
::echo %app%

::"start" suppresses the CMD window
start %python% %app%