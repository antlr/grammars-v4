# Tnsnames Grammar

## Summary

An ANTLR4 grammar for [Oracle 11g Release 2 tnsnames.ora specification document](http://docs.oracle.com/cd/E11882_01/network.112/e10835/tnsnames.htm) files.

This grammar is based on the details in the above document plus
a few seemingly undocumented bits and pieces that I know are still valid but
are not in the above document.

## Missing Features

This grammar has one missing feature, that I know about:

* The host names for the TCP protocol can be a DNS host name or an IP address.
Only IPv4 is acceptable at present.
I have yet to get my head around the format of an IPv6 address. (Sorry!)

Hey, this is my first ever proper grammar, so it's not too bad!

## Examples

There is an example parser, in the examples directory,
which allows you to parse a tnsnames.ora file looking for errors.
It is looking for syntax and semantic errors only.
It doesn't check that ports are in range, or that a correctly formatted IP address
has been supplied etc.

It works on normal entries for databases and also those entries for listeners
and scan listeners - at least, all the ones I've tried it out on.

There is a valid test file, tnsnames.test.ora, supplied if you want to have a play.
You'll need to edit a few problems into this file because at the moment, it is valid.

If you want a full blown tnsnames checker that _does_ do all that
parsing and checking and reporting errors etc, you can find
[Tnsnames checker utility on my blog](http://qdosmsq.dunbar-it.co.uk/blog/2014/12/tnsnames-checker-utility/).
Source code for full blown tnsnames parser utility is also available
on [GitHub](https://github.com/NormanDunbar/Tnsnames_checker).

## Contacts

Norman@dunbar-it.co.uk.


