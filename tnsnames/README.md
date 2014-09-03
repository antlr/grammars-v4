#Tnsnames Grammar

## Summary

An ANTLR4 grammar for [tnsnames](http://docs.oracle.com/cd/E11882_01/network.112/e10835/tnsnames.htm "Oracle's tnsnames.ora specification documnt") files.

This grammar is based on the details in the above document plus a few seemingly undocumented bits and pieces that I know are still valid but are not in the above document.

## Missing Features

This grammar has a couple of missing features, but not many:

* The host names for the TCP protocol can be a DNS host name or an IP address. Only IPv4 is acceptable at present. I have yet to get my head around the format of an IPv6 address. (Sorry!)

* The BEQ protocol's ARGS parameter is currently parsed as a single quoted string. I have tried breaking it into it's constituent parts, but I have yet to discover the correct grammar rules for it I'm afraid. (I'm working on it though...)


Hey, this is my first ever proper grammar, so it's not too bad!

Norman@dunbar-it.co.uk.





