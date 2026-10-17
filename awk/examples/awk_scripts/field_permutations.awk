#!/usr/bin/gawk -f
#
# script to output the permutations of a specific field/column from the csv file
#
# a value for variable "fieldnumber" should be passed to this script. it defines which
# field should be used for finding the permutations. alternatively, if a header row
# is present, "fieldname" may be specified. in this case the program will determine the
# position of the field.
#
# if you specify variable "noheader" as "1", you indicate that the file has NO header
# row otherwise the script will assume that there is a header row and thus calculate from
# the second row onwards.
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
# example: awk -f field_permutations -v fieldnumber=1 -v noheader=1 /home/test/datafile.csv
#          awk -f field_permutations -v fieldname=lastname -v quiet=1 /home/test/datafile.csv
#
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2011-04-12
#
#

# function to remove blanks on both sides of a string
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

	#x=systime();
	#print "it is: " strftime("%Y",x);

}

# adjust the fieldnumber if necessary before we process the lines of the file
NR <= 1 {
	# if we have a header row and the fieldname variable has been specified
	# we retrieve the position of the field from the first row.
	if(!noheader && fieldname)
	{
		for (i=1; i<=NF;i++)
		{
			if(tolower(fieldname)==tolower($i))
			{
				fieldnumber=i;
			}
		}
	}


	# adjust the field as required. if no fieldnumber was given the first field is used per default
	# if a fieldnumber is given higher than the number of fields of a row, the first field is used
	# per default
	if(fieldnumber==0 || fieldnumber > NF)
	{
	  fieldnumber=1;
	}
	fieldname=$fieldnumber;
}

# we skip the first header row as a default, unless the noheader variable is set to "1"
NR > 1 || noheader==1 {
	# in case the field does not contain a value, we assign a default value of [empty]
	if($fieldnumber=="") $fieldnumber="[empty]";
	if(ignorecase==1) $fieldnumber=tolower($fieldnumber)
	
	# increase the counter variable of the field in the array
	++dimension[trim($fieldnumber)];
}

# after all rows are processed we output the summary
END {
	# give out which field we are using
	if(quiet!=1)
	{
	  print "evaluated field: " fieldname " (column=" fieldnumber ")";
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

	# we could sort the array as well but can also be done with the sort command on the shell
	#x=asorti(dimension,dimension2);	
	#for (i = 1; i <= x; i++)
	#{
		#print dimension2[i] ": " dimension[dimension2[i]];
	
	#}

	
	# give out the number of lines that we processed
	if(quiet!=1)
	{
	  print "";
	  print "evaluated field:\t\t " fieldname " (column=" fieldnumber ")";
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

