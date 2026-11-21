#!/usr/bin/awk -f
#
# script to output the permutations of two specific fields/columns from the csv file
#
# a value for variable "fieldnumber1" shall be passed to this script. 
# a value for variable "fieldnumber2" shall be passed to this script. the two numbers define
# which two fields should be used for finding the permutations.
#
# if you specify variable "noheader" as "1", you indicate that the file has NO header
# row
#
# if you specify "quiet" equal to "1", you indicate that no output shall be generated except
# displaying the permutations that were found.
#
# if you specify "ignorecase" equal to "1", you indicate that the list of permutations should
# ignore the case of the field content. e.g. "Hello" and "hello" would be added up together.
#
# you may specify "nonumbers" equal to "1", indicating that no counts of the permutations
# shall be displayed.
#
# example: awk -f 2field_permutations -v fieldnumber1=1 -v fieldnumber2=3 -v noheader=1 /home/test/datafile.csv
#
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-01-21
#
#

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
}

# adjust the fieldnumber if necessary before we process the lines of the file
NR <= 1 {
	# adjust the field as required. if no fieldnumber was given the first field is used per default
	# if a fieldnumber is given higher than the number of fields of a row, the first field is used
	# per default
	if(fieldnumber1==0 || fieldnumber1 > NF)
	{
	  fieldnumber1=1;
	}
	fieldname1=$fieldnumber1;

	if(fieldnumber2==0 || fieldnumber2 > NF)
	{
	  fieldnumber2=1;
	}
	fieldname2=$fieldnumber2;
}

# we skip the first header row as a default, unless the noheader variable is set to "1"
NR > 1 || noheader==1 {
	# in case the field does not contain a value, we assign a default value of [empty]
	if($fieldnumber1=="") $fieldnumber2="[empty]";
	if($fieldnumber2=="") $fieldnumber2="[empty]";
	if(ignorecase==1) $fieldnumber1=tolower($fieldnumber1)
	if(ignorecase==1) $fieldnumber2=tolower($fieldnumber2)
	
	# add the value of the two fields to the array
	++dimension[$fieldnumber1 " :: " $fieldnumber2];
}

# after all rows are processed we output the summary
END {
	# give out which field we are using
	if(quiet!=1)
	{
	  print "evaluated fields: " fieldname1 "(" fieldnumber1 ")" " :: " fieldname2 "(" fieldnumber2 ")" ;
	  print "";
	}
	
	# loop over the items in the array
	for (item in dimension)
	{
		# count the number of items of the array
		++counterArray;
		# print out item and the number of occurrences
		if(!nonumbers)
		{
			print item ": " dimension[item];
		}
		else
		{
			print item;
		}

	}
	
	# give out the number of lines that we processed
	if(quiet!=1)
	{
	  print "";
	  print "evaluated fields:\t\t " fieldname1 "(" fieldnumber1 ")" " :: " fieldname2 "(" fieldnumber2 ")" ;
	  print "number of permutations:\t\t " counterArray;
	  if(noheader==1)
	  {
		totalnumberoflines = NR;
	  }
	  else
	  {
		totalnumberoflines = NR -1
	  }
	  print "total lines (w/o header):\t " totalnumberoflines;
  	  printf "permutations to total lines:\t %3.2f %\n", counterArray * 100 / totalnumberoflines;
	}
}

