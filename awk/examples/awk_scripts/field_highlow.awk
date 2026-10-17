#!/usr/bin/awk -f
#
# script to output high and low values of a selected field. you may use numbers or strings
# strings will be sorted in alphabetical order.
#
# a value for variable "fieldnumber" should be passed to this script. it defines which
# field should be used as the dimension field/column
#
# if you specify variable "noheader" as "1", you indicate that the file has NO header
# row
#
# if you specify "quiet" as "1", you indicated that no output should be generated.
#
# example call of this script:
#
# field_highlow.awk -v fieldnumber=7 -v noheader=1 /home/testuser/testfile.csv
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2009-05-11
#
#

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
	counter = 0;
	lowestValue=0;
	highestValue=0;
}

# first line is the header row. we retrieve the name of the selected field.
NR <= 1 {
	fieldname = $fieldnumber;
}

# we skip the first header row and only take those rows matching with the query parameter
(NR > 1 || noheader==1) {
	# in case the does not contain a value, we ignore it
	if(ignorecase==1)$fieldnumber=tolower($fieldnumber);
	if($fieldnumber!="")
	{
		if(($fieldnumber>highestValue) || highestValue==null)
		{
			highestValue = $fieldnumber;
		}	
		if(($fieldnumber<lowestValue) || lowestValue==null)
		{
			lowestValue = $fieldnumber;
		}	
	}
}

END {
	if(quiet!=1)
	{
	  print "";
	  # give out which field we are using
	  print "evaluated field: " fieldname;
	  print "high value:      " highestValue;
	  print "low value:       " lowestValue;

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

