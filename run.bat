@echo off

set startTime=%time%
echo compile
ghc -e "import System.Environment" -e ":set args compile" -e main Main.hs

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
