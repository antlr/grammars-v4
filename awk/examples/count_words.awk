#!/usr/bin/awk -f
BEGIN {
  print "Processing file..."
}
{
	if (NF > 0) {
		# remove leading and trailing whitespace
		gsub(/^[[:space:]]+|[[:space:]]+$/, "", $0)
		# convert all characters to lowercase
		$0 = tolower($0)
		# split the line into words
		split($0, words, /[[:space:]]+/)
		# print out the number of words in the line
		print "Line", NR, "contains", length(words), "words."
	}
}
END {
  print "Finished processing file."
}
