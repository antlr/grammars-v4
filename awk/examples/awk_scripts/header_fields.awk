#!/usr/bin/awk -f
#
# script to output the fields and positions of the first row
#
#
# example: awk -f header_fields /home/test/datafile.csv
#
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2009-11-02
#
#

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
}

NR==1 {

	for(i=1;i<=NF;i++)
	{
		printf "%+24s %4i\n", $i, i
	}
}


