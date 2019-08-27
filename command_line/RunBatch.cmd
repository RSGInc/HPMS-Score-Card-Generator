@ECHO OFF

REM RunBatch.cmd
REM 23 Oct 2018, matt.landis@rsginc.com
REM Calls R scripts for creating HPMS scorecards


REM Use variables locally
REM Enable command processor extensions
SETLOCAL ENABLEEXTENSIONS

REM Get the name of this file
SET me=%~n0
REM Get the parent directory of this file
SET parent=%~dp0

REM create a log file named [script].YYYYMMDDHHMMSS.txt
IF NOT EXIST "%parent%..\output\" mkdir %parent%..\output\
SET log_base=_%me%_log_%DATE:~10,4%%DATE:~4,2%%DATE:~7,2%_%TIME:~0,2%%TIME:~3,2%%TIME:~6,2%.txt
SET log_base=%log_base: =0%
SET log=%parent%..\output\%log_base%

ECHO -----------------------------
ECHO Writing to log file at "%log%"

REM Get the arguments
SET states=%*

REM Call the GetStates script to write do_states.csv
CALL :tee Getting available states with getStates.R
Rscript getStates.R %states% >> "%log%" 2>&1

REM Call the RunState script
FOR /f %%a IN (do_states.csv) DO (
  ECHO Running runState.R for %%a
  Rscript runState.R %%a >> "%log%" 2>&1
)

ECHO -----------------------------------
ECHO Finished!
ECHO Check log file at %log%

del do_states.csv

:END

ENDLOCAL
ECHO ON
@EXIT /B 0

REM create a tee function for writing to the console and a file
REM http://steve-jansen.github.io/guides/windows-batch-scripting/part-7-functions.html
:tee
ECHO %* >> "%log%"
ECHO %*
EXIT /B 0
