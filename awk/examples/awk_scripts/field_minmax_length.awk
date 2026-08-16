#!/usr/bin/awk -f
#
# script to output the min and max length of selected field.
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
# field_minmax_length.awk -v fieldnumber=7 -v noheader=1 /home/testuser/testfile.csv
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2009-05-14
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
	counter = 0;
	minLength=0;
	minValue="[]"
	maxLength=0;
	maxValue="[]";
}

# first line is the header row. we retrieve the name of the selected field.
NR <= 1 {
	fieldname = $fieldnumber;
}

# we skip the first header row and only take those rows matching with the query parameter
(NR > 1 || noheader==1) {
	# if the field is longer than the current longest one, set the maxLength
	# variable to the new value	
	if((length(trim($fieldnumber))>maxLength) || maxLength==null)
	{
		maxLength = length(trim($fieldnumber));
		maxValue = $fieldnumber;
	}	
	# if the field is smaller than the current smallest one, set the minLength
	# variable to the new value	
	if((length(trim($fieldnumber))<minLength) || minLength==null)
	{
		minLength = length(trim($fieldnumber));
		minValue = $fieldnumber;
	}	
}

END {
	if(quiet!=1)
	{
	  print "";
	  # give out which field we are using
	  print "evaluated field: " fieldname;
	  print "minimum length:  " minLength " : " minValue;
	  print "maximum length:  " maxLength " : " maxValue;

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

