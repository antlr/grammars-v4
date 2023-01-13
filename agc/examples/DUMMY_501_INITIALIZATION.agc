### FILE="Main.annotation"
# Copyright:	Public domain.
# Filename:	DUMMY_501_INITIALIZATION.agc
# Purpose:	Part of the source code for Solarium build 55. This
#		is for the Command Module's (CM) Apollo Guidance
#		Computer (AGC), for Apollo 4.
# Assembler:	yaYUL --block1
# Contact:	Jim Lawton <jim DOT lawton AT gmail DOT com>
# Website:	www.ibiblio.org/apollo/index.html
# Page scans:	www.ibiblio.org/apollo/ScansForConversion/Solarium055/
# Mod history:	2009-09-14 JL	Created.

## Page 697
		BANK	33
BEGINNER	TC	BANKCALL
CADRMODE	CADR	IMUREENT

BEGIN501	TC	INTPRET
		VMOVE	1
		ITC
			RN
			CALCGRAV

		EXIT	0
		TC	PHASCHNG	# SETUP SOME PHASE INFO.
EXITLOC2	OCT	00105		# 5.1 MODE GOES WITH READACCS.

		INHINT
		CS	TIME1
		AD	STARTDT1
		TC	WAITLIST
		CADR	READACCS

		CS	TIME1		# SPARE START ROUTINE
		AD	STARTDT2
		TC	WAITLIST
		CADR	START2

		TC	ENDOFJOB

BEGINSW		TC	BANKCALL	# WAIT FOR MODE SWITCH IF NECESSARY.
		CADR	IMUSTALL
		TC			## (JL) seems to be an arg missing. Is YUL assuming 0? Generates 07435.

		TC	ENDOFJOB

		DEC	0
STARTDT1	DEC	200
STARTDT2	DEC	830

START2		CAF	PRIO27
		TC	FINDVAC
		CADR	S4BSMSEP

		TC	TASKOVER
