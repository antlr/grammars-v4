#!/usr/bin/awk -f
#
# script to sum up the values of a selected field/column. string values will be ignored
#
# a value for variable "fieldnumber" must be passed to this script. it defines which
# field should be used as the dimension field/column
#
# if you specify variable "noheader" as "1", you indicate that the file has NO header
# row
#
# if you specify "quiet" as "1", you indicated that no output should be generated.
#
# example call of this script:
#
# field_sum.awk -v fieldnumber=7 -v noheader=1 /home/testuser/testfile.csv
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-03-24
#
#

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
	counter = 0;
	sum=0;
	fieldname=fieldnumber;
}

# first line is the header row. we retrieve the name of the selected field.
NR <= 1 && !noheader {
	fieldname = $fieldnumber;
}

# we skip the first header row and only take those rows matching with the query parameter
(NR > 1 || noheader==1) {
	# in case the does not contain a value, we ignore it
	if($fieldnumber!="")
	{
		sum = sum + $fieldnumber;
	}
}

END {
	if(quiet!=1)
	{
	  print "";
	  # give out which field we are using
	  print "evaluated field: " fieldname;
	  print "sum:             " sum;
	  # give out the number of lines that we processed
	  if(noheader==1)
	  {
		totalnumberoflines = NR;
	  }
	  else
	  {
		totalnumberoflines = NR -1
	  }
	  print "total lines (w/o header): " totalnumberoflines;
	}
}

