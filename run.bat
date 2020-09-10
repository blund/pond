@echo off

set startTime=%time%
echo compile
make compile

echo Start compile: %startTime%
echo End compile:   %time%


set startTime=%time%
echo.
echo assemble
gcc -m32 test.s

echo Start assemble: %startTime%
echo End assemble:   %time%

echo execute
a.exe

echo.
echo result:
echo %ERRORLEVEL%
del test.s
