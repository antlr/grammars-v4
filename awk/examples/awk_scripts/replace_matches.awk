#!/usr/bin/awk -f
#
# script loads a file containing words and their subtitutions
# (same syntax as property files). then a specific field of the input
# file is replaced with the relevant substitution, if it exists.

# examples from substitutions file:
# create a file with key/value entries e.g. "subst.txt", one entry per row such as:
#
# EUR=Europe
# USA=United States of America
# MEA=Middle East
#
# when you run this script pass the name of the file with the substitutions
# to it.
# you also need to specify which field (column) of the input file you want
# to have checked and substituted.
#
# example input file:
#
# hallo;blue;winter;EUR;fifteen
# hello;red;summer;USA;three
# haji;yellow;summer;AFR;ninehundred
#
# save this example as "inputfile.csv". then run this awk script from the appropriate
# folder as follows:
#
# ./replace_matches.awk -v fieldnumber=4 -v substitutionfile=subst.txt inputfile.csv
#
# process description: the script reads the entries of the substitution file subst.txt
# and replaces all occurrences in field/column number 4 in the inputfile.
# so e.g. all entries that have the value "EUR" in column 4 of the input file will
# be changed to read "Europe" and all "USA" values in column 4 will be replaced with
# "United States of America" instead.
#
# Note: be careful not to replace existing data with data that contains the seperator
#       character (that is used to devide the individual columns from each other).
#       otherwise you will create output, where the rows might have different numbers of
#       columns.
#
# Note: watch out in the substitution file that the devider used to seperate the key from
#       the value does not appear elsewhere in the data. e.g. if you use a "=" as the
#       devider, it should not appear in the data itself otherwise you might get undesired
#       results.
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-01-14
#
#

# before we processs the main file, we load the matching file into memory in an array.
BEGIN {
	# define input and output field seperators
	FS=";";
	OFS=";";
	if(devider) # devider between key and value paris in the substitutions file
	{
		substitutions_devider=devider;
	}
	else
	{
		substitutions_devider="=";	
	}
	# loop over all lines in the substitution file
	while ( (getline substitution < substitutionfile) > 0 )
	{
		# get the position of the devider		
		pos = index(substitution,substitutions_devider);
		# get the first part of the line, which will be the key in the
		# array that will be used for the sustitutions
		key=substr(substitution,1,pos-1);
		if(key !="" && substr(key,1,1)!="#") # only if not empty and not a comment
		{
			# value holds the actual substitution
			value=substr(substitution,pos+1);
			# add to the associative array
			entry[key]=value;
		}
	}
}

# the following will be executed for each row of the input file.
{
	# the replacement_value is the value that will be used instead of the
	# original one, which is in the given fieldnumber (column).	
	replacement_value=entry[$fieldnumber]
	# we replace the value only, if we found a match to it in the array
	if(replacement_value)
	{
		# counter++;
		# print "line " NR ": [" $fieldnumber "] replaced by [" replacement_value "]";
		# replace the original value of the field with the one that matched
		$fieldnumber = replacement_value;
	}
	else	# if we do not have a match in the array, this means that for the given value
		# in the input file there is no equivalent substitution value defined in the
		# substitution file.
	{
		#counter_notmatched++;		
#		print "line " NR ": [" $fieldnumber "] not in array";
	}
	print;

}

