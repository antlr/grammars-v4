#!/usr/bin/awk -f
#
# script to find out the position (column) of a selected field. the case of
# the fields is ignored.
#
# a value for variable "fieldname" must be passed to this script. it defines which
# field/column should be used
#
# example call of this script:
#
# field_query_header.awk -v fieldname=country /home/testuser/testfile.csv
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-02-02
#
#

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
	fieldnumber="[not found]";
}

# first line is the header row. we retrieve the number of the selected field
NR <= 1 {
		for (i=1; i<=NF;i++)
		{
			if(tolower(fieldname)==tolower($i))
			{
				fieldnumber=i;
			}
		}
}

END {
	  # give out which field we are using
	  print "evaluated field name: " fieldname;
	  print "column in header row: " fieldnumber;
}

