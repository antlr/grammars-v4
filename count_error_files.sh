#!/bin/sh

find . * | grep examples/ | grep .error | wc -l

