#!/usr/bin/awk -f
#
# script to trim space from beginning and end of each field.
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-02-02
#
#

# function to remove blanks on both sides of the string
function trim(value) 
{
    sub(/^ */,"",value)
    sub(/ *$/,"",value)
    return value
} 

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
	OFS=";";
}

{
	for(i=1;i<=NF;i++)
  	{
        	$i=trim($i);
	}
	print;
}


