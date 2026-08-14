#!/usr/bin/awk -f
#
# script that capitalizes all words. at the same time sets all other characters of the
# words to lowercase.
#
# uwe.geercken@datamelt.com
# http://datamelt.com
#
# last update: 2009-05-11
#
#

function capitalizeWords(textstring)
{
	# we assume that the text string is seperated by spaces	
	split(textstring,words," ");
	newstring=""
	# loop over the members of the array
	for ( item in words ) 
	{
	   # we capitalize the first letter of the word and then add the rest
	   # of the word in lowercase
           newWord= toupper(substr(words[item],1,1)) tolower(substr(words[item],2))
	   newstring=newstring " " newWord;
	}
	return newstring;
}

BEGIN {
	OFS=FS=";";
}

{
	split(exceptions,exc,",");	
	# loop over all fields/columns of the input stream
	for(i=1;i<=NF;i++)
  	{
		result=exc[i];
		print "xxx: " result;
		if(result=="")
		{
			$i=capitalizeWords($i);
		}
	}
	print;
}
