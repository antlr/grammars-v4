#!/usr/bin/awk -f
#
# script to output those lines that correspond to a selected field and
# query string.
#
# a value for variable "fieldnumber" should be passed to this script. it defines which
# field should be used as the dimension field/column
#
# a value for variable "query" may be passed to this script. it defines a value that
# the above defined field should match to to be selected. A matching of the given
# query string to the file is done based on regular expression matching rules.
#
# if you specify variable "noheader=1", you indicate that the file has NO header
# row
#
# if you specify "quiet=1", you indicated that no output should be generated except
# for the rows found that match the given query string
#
# example call of this script:
#
# field_query.awk -v fieldnumber=7 -v query=Germany -v noheader=1 /home/testuser/testfile.csv
#
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2010-11-02
#
#

# begin of processing
BEGIN {
	# setting the file's field seperator
	FS=";";
	counter = 0;
}

# first line is the header row. we retrieve the name of the selected field.
NR <= 1 {
	fieldname = $fieldnumber;
}

# we skip the first header row and only take those rows matching with the query parameter
(NR > 1 || noheader==1) && $fieldnumber ~ query {
	print $0;
	++counter;
}

END {
	if(quiet!=1)
	{
	  print "";
	  # give out which field we are using
	  print "evaluated field: " fieldname;
	  print "query value:     " query;
	  print "fetched lines:   " counter;

	  # give out the number of lines that we processed
	  print "total lines (w/o header): " NR-1;
	}
}

