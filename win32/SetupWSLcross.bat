@echo off
rem Setup MCL and echo the environment
rem Usage: eval `cmd.exe /c SetupWSLcross.bat x64`

IF "%~1"=="x86" GOTO search
IF "%~1"=="x64" GOTO search

GOTO badarg

:search

rem Looking through all potential drives
set drives=A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z

FOR %%x in (%drives%) do (
IF EXIST "%%x:\Program Files\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvarsall.bat". (
   call "%%x:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat". (
   call "%%x:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" %~1 > nul
   goto continue
)

IF EXIST "%%x:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat". (
   call "%%x:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" %~1 > nul
   goto continue
)
)

GOTO no_vcvars

:continue
FOR /F "delims==" %%F IN ('where cl.exe') DO SET _cl_exec_=%%F
FOR %%F IN ("%_cl_exec_%") DO SET CL_PATH=%%~dpF

FOR /F "delims==" %%F IN ('where rc.exe') DO SET _rc_exec_=%%F
FOR %%F IN ("%_rc_exec_%") DO SET RC_PATH=%%~dpF

rem Order is important for some unknown reason
set WSLENV=VCToolsRedistDir/up:CL_PATH/up:RC_PATH/up:LIBPATH/ul:LIB/ul:INCLUDE/ul
wsl.exe echo INCLUDE=\"$INCLUDE\";
wsl.exe echo LIB=\"$LIB\";
wsl.exe echo LIBPATH=\"$LIBPATH\";
wsl.exe echo VCToolsRedistDir=\"$VCToolsRedistDir\";
wsl.exe echo PATH=\"$CL_PATH\":\"$RC_PATH\":'$PATH';
wsl.exe echo WSLENV='$WSLENV:LIBPATH/l:LIB/l:INCLUDE/l';
wsl.exe echo WSLcross=true;
wsl.exe echo export 'INCLUDE LIB LIBPATH VCToolsRedistDir WSLENV PATH WSLcross';
wsl.exe echo "# Eval this file eval \`cmd.exe /c SetupWSLcross.bat\`"

rem done
exit

:badarg
echo "Bad TARGET or not specified: %~1 expected x86 or x64"
exit

:no_vcvars
echo "Error: SetupWSLcross.bat: Could not find vcvarsall.bat"
exit
