@echo off
java.exe -jar antlr-4.9.2-complete.jar  ..\..\pggrammar.g4 ..\..\pglexer.g4
if exist ..\..\*.tokens del ..\..\*.tokens
if exist ..\..\*.interp del ..\..\*.interp
