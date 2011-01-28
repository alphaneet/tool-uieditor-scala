@echo off

if not exist bin mkdir bin
call scalac -d bin *.scala
call scala -classpath bin neet.tool.editor.ui.Main
