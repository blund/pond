@echo off

make compile
gcc -m32 test.s
a.exe
echo %ERRORLEVEL%
del test.s
