#! /bin/bash

echo Start ANTLR
antlr4 TJS*.g4
echo Start javac
cp Java/* .
javac TJS*.java
