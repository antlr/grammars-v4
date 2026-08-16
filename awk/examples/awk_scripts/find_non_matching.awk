#!/usr/bin/awk -f
#
# script matches expressions/words from a matching file against a field of the input file
#
# when you run this script pass the name of the file with the list of words to match
# to it. you also need to specify which field (column) of the input file you want
# to have checked. fields need to e seperated by a semicolon (;)
#
# example usage:
#
# ./find_non_matching.awk -v fieldnumber=4 -v matchingfile=match.txt inputfile.csv
#
# or
#
# ./find_non_matching.awk -v nonumbers=1 -v fieldnumber=4 -v matchingfile=match.txt inputfile.csv
#
# or
#
# ./find_non_matching.awk -v reverse=1 -v fieldnumber=4 -v matchingfile=match.txt inputfile.csv
#
# process description: the script reads the entries of the matchfile match.txt
# and looks in each row of the input file for an equivalent value in the field/column specified.
# at the end of the script the non-matched values will be output.
#
# you may reverse the process by looking for data that matches instead of looking for non-matching
# data by specifying a value for the reverse variable (-v reverse=1)
#
# specify the variable noheader=1 if the input file has no header row. per default the process will
# skip the first row.
#
#
# note: if you don't want to have the number of occurrence of each non-match value displayed
#       then pass a variable of "nonumbers=1" to the script 
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-07-27
#
#

# before we processs the main file, we load the matching file into memory in an array and
# set some variables;
BEGIN {
	# define input and output field seperators
	FS=";";
	OFS=";";
	# counter for total input lines
	totalcounter=0;
	#counter for nonmatching/matching values
	counter=0;
	# counter for entries in the matching file
	entrycounter=0;
	
	# check if a matching file was specified otherwise exit the script
	if(!matchingfile)
	{
		print "\nerror: no matching file specified";
		exit 1;
	}
	
	# check if a fieldnumber variable was specified otherwise
	# use the first column
	if(!fieldnumber || fieldnumber==0)
	{
		fieldnumber=1;
	}
	
	# loop over all lines in the matching file
	# and add value to an array
	while ( (getline matching < matchingfile) > 0 )
	{
		if(matching !="" && substr(matching,1,1)!="#") # only if not empty and not a comment
		{
			# add the value to the array
			entrycounter++;
			++entry[tolower(matching)];
		}
	}
}

# we skip the first header row as a default, unless the noheader variable is set to "1"
NR > 1 || noheader==1 {
	totalcounter++;
	found_value=entry[tolower($fieldnumber)]
	#print "checking for: " $fieldnumber;
	
	# in normal mode and if the value is not matched, we keep it
	# in reverse mode we keep all that match
	if((!reverse && !found_value) && $fieldnumber!="" || (reverse && found_value && $fieldnumber!="" ))
	{
			counter++;		
			++foundarray[$fieldnumber];
	}
}

END {
	print "";
	# output will be the value that could not be matched and the number of occurrence in
	# the input file
	for (item in foundarray)
	{
		if(!nonumbers)
		{
			print item ": " foundarray[item];
		}
		else
		{
			print item;
		}
	}
	print ""
	print "total entries in matching file:         " entrycounter;
	print "total lines in input file:              " totalcounter;
	print "total elements found in input file:     " length(foundarray);
	if (reverse)
	{
		print "total matching lines in input file:     " counter;
	}
	else
	{
		print "total non-matching lines in input file: " counter;
	}
}

