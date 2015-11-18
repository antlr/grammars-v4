# Copyright:	Public domain.
# Filename:	Makefile
# Purpose:	Makefile for Artemis build 072.
#		(The source code for the Command Module's (CM)
#		Apollo Guidance Computer (AGC), for Apollo 15-17
# Contact:	Ron Burkey <info@sandroid.org>.
# Mod history:	2005-07-06 RSB 	Began.
#		2006-01-09 RSB  Removed the comment to ignore 
#				errors.  (Artemis072.binsource is 
#				now complete and proofed, so the
#				warning isn't needed.)
#		2009-07-04 RSB	Added --html switch.

PROJECT:=Artemis072

ifndef PREFIX
PREFIX=/usr/local
endif

SOURCE:=$(wildcard *.agc)
TARGETS=${PROJECT}.bin ${PROJECT}.lst

default: $(TARGETS)

clean:
	-rm -f *.lst *~ *.bin *.symtab *.html
	
install: default
	cp ${PROJECT}.bin ${PREFIX}/bin
	chmod ugo+r ${PREFIX}/bin/${PROJECT}.bin
	
# Implicit rules.

%.bin: %.binsource
	../Luminary131/Oct2Bin <$^
	mv Oct2Bin.bin $@

%.lst: %.bin ${SOURCE}
	@echo ====================================================
	@echo Errors in the following are normal, since Artemis072
	@echo is under development.
	-../yaYUL/yaYUL --html MAIN.agc >$@
	-diff -s MAIN.agc.bin $^
		

