### FILE="Main.annotation"
# Copyright:	Public domain.
# Filename:	MAIN.agc
# Purpose:	Part of the source code for Solarium build 55. This
#		is for the Command Module's (CM) Apollo Guidance
#		Computer (AGC), for Apollo 4.
# Assembler:	yaYUL --block1
# Contact:	Ron Burkey <info@sandroid.org>
# Website:	www.ibiblio.org/apollo/index.html
# Page scans:	www.ibiblio.org/apollo/ScansForConversion/Solarium055/
# Mod history:	2009-07-25 RSB	Adapted from corresponding Colossus 249 file.
# 		2009-09-14 JL	Fixed page number.
#
# This file is a little different from the other Solarium055 files being provided, 
# in that it doesn't represent anything that appears directly in the original source.  
# What I (RSB) have done for organizational purposes is to split the huge monolithic
# source code into smaller, more manageable chunks--i.e., into individual source 
# files.  Those files are rejoined within this file as "includes".  It just makes
# it a little easier to work with.  The code chunks correspond to natural divisions
# into sub-programs as indicated by the page headings in the assembly listing. 

# Module Filename				Starting page
# ---------------				-------------

$ASSEMBLY_AND_OPERATION_INFORMATION.agc		# 1
$ERASABLE_ASSIGNMENTS.agc			# 12
$INTERRUPT_TRANSFER_ROUTINES.agc		# 35
$FIXED-FIXED_INTERPRETER_SECTION.agc		# 37
$BANK_03_INTERPRETER_SECTION.agc		# 70
$EXECUTIVE.agc					# 98
$WAITLIST.agc					# 111
$RESTART_CONTROL.agc				# 120
$501_RESTART_TABLES_AND_ROUTINES.agc		# 125
$FRESH_START_AND_RESTART.agc			# 140
$DOWN-TELEMETRY_PROGRAM.agc			# 151
$T4RUPT_OUTPUT_CONTROL_PROGRAMS.agc		# 159
$MODE_SWITCHING_AND_MARK_ROUTINES.agc		# 190
$IMU_COMPENSATION_PACKAGE.agc			# 214
$IRIG_PULSE-TORQUING_ROUTINES.agc		# 224
$EXTENDED_VERBS_FOR_MODING.agc			# 235
$AGC_SELF_CHECK.agc				# 265
$INTER-BANK_COMMUNICATION.agc			# 290 
$ALARM_AND_DISPLAY_PROCEDURES.agc		# 293
$ORBITAL_INTEGRATION_PROGRAM.agc		# 296
$ORBITAL_INTEGRATION_FOR_501.agc		# 326
$PRELAUNCH_ALIGNMENT_PROGRAM.agc		# 332
$INFLIGHT_ALIGNMENT_PROGRAM.agc			# 366
$RTB_OP_CODES.agc				# 374
$IMU_PERFORMANCE_TESTS_1.agc			# 392
$IMU_PERFORMANCE_TESTS_2.agc			# 426
$INFLIGHT_ALIGNMENT_SUBROUTINES.agc		# 461
$KEYRUPT_UPRUPT_FRESH_START.agc			# 477
$PINBALL_GAME_BUTTONS_AND_LIGHTS.agc		# 481
$501_MISSION_CONTROL_PROGRAM.agc		# 553
$POWERED_FLIGHT_SUBROUTINES.agc			# 628
$TIME_OF_FREE-FALL_CALCULATIONS.agc		# 682
$DUMMY_501_INITIALISATION.agc			# 697
$RE-ENTRY_CONTROL.agc				# 698
$AVERAGE_G_INTEGRATOR.agc			# 746
$VERIFICATION_ASSISTANCE_PROGRAMS.agc		# 754
$SUM-CHECK_END_OF_RECORD_MARKS.agc		# 759
#Assembly-tables				# 762


