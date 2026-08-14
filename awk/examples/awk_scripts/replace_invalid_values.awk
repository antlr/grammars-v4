#!/usr/bin/awk -f

#
# script to replace empty or null value fields that
# are unusable/unwanted, with a default value
#
# the result is an output file that does not contain any
# empty or null values anymore
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2009-05-11
#
#

BEGIN {
	
	# the field seperator used during input
	FS=";";	
	# the output field seperator
	OFS=";";

	# the default value for all invalid or empty values
	DEFAULT_VALUE = "[undefined]";
}

# valid for all records of the input file - no filtering his done here
{
	# we want to replace all empty fields of each row with the default value
	
	# loop over the fields. awk starts counting at 1!
	for(f=1;f<=NF;f++) 
	{

		# check if one of the conditions is true
		if($f == "null" || $f == "NULL" || $f == "Null" || $f == "" || $f == " " || $f == null || $f ~ /^~/)
		{
			# replace the invalid value with the default one			
			$f = DEFAULT_VALUE;
		}
	}

	# print the record
	print $0;	

}

