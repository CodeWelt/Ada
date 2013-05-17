@ECHO OFF
rem $Id: adamake.bat 143 2006-11-20 22:57:25Z keulsn $

rem You can add this file to your path, by copying it into the "bin" 
rem directory of your local gnat installation. You have to make sure that 
rem this directory is in your path. You can check this by typing 'PATH' 
rem (without the apostrophes) in your command-shell, and check if the
rem directory is listed there.

rem Check if file was passed...
IF [%1]==[] GOTO E_FLAG

@ECHO ON
gnatmake -f -g -gnata -gnato -gnaty3acefhiklmnprt "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
@ECHO OFF
goto ende

:E_FLAG
@ECHO ON
gnatmake -help
@ECHO OFF


:ende