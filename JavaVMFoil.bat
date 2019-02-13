@echo off

:: first procedure argument should be the path to the main directory
set basepath=C:\dia\
if not {%1}=={} set basepath=%1
pushd %basepath%

:: if Foil server process exists, kill it
:: PsList, an utility from http://www.sysinternals.com/
PsList|find /I "Java" >NUL && (echo Killing Java & goto :KillJava)
goto :java
:killJava
PsKill java.exe
:: needed
sleep 1

:Java
echo starting Foil RuntimeServer (JVM)
set ports=13578 13579

set swtpath=%basepath%
set foilpath=%basepath%foil\

@echo on
start "Foil Server %ports%" /high /min ^
java -cp %foilpath%;%swtpath%swt.jar ^
-Djava.library.path=%swtpath% ^
com.richhickey.foil.RuntimeServer %ports%
@echo off


