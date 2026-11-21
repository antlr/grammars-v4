#!/usr/bin/awk -f
#
# script to determine the min and max number of fields/columns over all records.
#
# useful to analyse if there are inconsistancies in a file or the other way
# around to make sure there are no inconsistancies.
#
# example call of this script:
#
# number_of_fields_min_max.awk /home/testuser/testfile.csv
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
	lowestValue=0;
	highestValue=0;
}

{
	if((NF > highestValue) || highestValue==null)
	{
		highestValue = NF;
	}	
	if((NF < lowestValue) || lowestValue==null)
	{
		lowestValue = NF;
	}	

}

END {
	  # give out which field we are using
	  print "number of records:             " NR;
	  print "number of fields - high value: " highestValue;
	  print "number of fields - low value:  " lowestValue;
}

