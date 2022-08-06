#!/bin/sh

test=""
files=`find . -name pom.xml | grep -v Generated | grep -v -E -e "save/|target/"`
refined_list=`grep -l -i -e entrypoint $files`
ls $refined_list | wc