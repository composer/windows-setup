@ECHO OFF

rem Params:
rem   $1      working directory (the selected folder)
rem   $2...   composer params

set dir=%~1
shift
set cmd=%1
set params=

:Loop
if not "%~1" == "" (
    set params=%params% %1
    shift
    goto Loop
)

cd %dir%
if ERRORLEVEL 1 goto Exit
if not "%cmd%" == "--open--" (
    echo.
    echo Command: composer %params%
    echo.
    call composer %params%
    goto Exit
)


:Exit
if "%cmd%" == "--open--" goto Continue
goto Ask


:Ask
echo.
set /p result= Continue [y] ?
if "%result%" == "y" goto Continue
if "%result%" == "Y" goto Continue
exit

:Continue
echo.
