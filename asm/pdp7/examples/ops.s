" ops and system calls from as7; as.s has no initial symbol table
" so this needs to be included in any assembly of user programs

" "sys" has it's own sop.s which includes I/O instructions, but lacks
" system call definitions, and lacks some instructions used in
" commands but not in the kernel, so it can't have been the file used
" for user assemblies.

save   = 1		" saves core dump & user area!
getuid = 2
open   = 3
read   = 4
write  = 5
creat  = 6
seek   = 7
tell   = 8
close  = 9
link   = 10
unlink = 11
setuid = 12
rename = 13
exit   = 14
time   = 15
intrp  = 16
chdir  = 17
chmod  = 18
chown  = 19
" 20 removed
sysloc = 21		" return system addresses
" 22 removed
capt   = 23		" capture display?
rele   = 24		" release display?
status = 25		" "stat"
smes   = 27
rmes   = 28
fork   = 29

" List of instruction names and machine code values
" These come from https://raw.githubusercontent.com/simh/

sys    = 0020000		" "cal i" instruction (trap indirect thru 020)
i      = 0020000		" indirect bit

" memory reference instructions
dac    = 0040000		" deposit AC
jms    = 0100000		" jump to subroutine
dzm    = 0140000		" deposit zero in memory
lac    = 0200000		" load AC
xor    = 0240000		" exclusive or
add    = 0300000		" one's complement add
tad    = 0340000		" two's complement add
xct    = 0400000		" execute
isz    = 0440000		" increment and skip if zero
and    = 0500000		" AND with contents of Y
sad    = 0540000		" skip if AC different from content of Y
jmp    = 0600000		" jump to Y

" Type 177 Extended Arithmetic Element (EAE)
eae    = 0640000		" base instruction (nop)
osc    = 0640001		" OR SC into AC
omq    = 0640002		" OR MQ into AC
cmq    = 0640004		" Complement MQ
div    = 0640323		" divide
norm   = 0640444		" normalize unsigned
lls    = 0640600		" long left shift
als    = 0640700		" AC shift
lrs    = 0640500		" long right shift
lacs   = 0641001		" load AC with SC
lacq   = 0641002		" load AC with MQ
abs    = 0644000		" absolute value
divs   = 0644323		" divide signed

clq    = 0650000		" clear MQ
frdiv  = 0650323		" fractional divide
lmq    = 0652000		" load MQ from AC
mul    = 0653122		" multiply
idiv   = 0653323		" integer divide
idivs  = 0657323		" integer divide signed
frdivs = 0654323		" fractional divide signed
muls   = 0657122		" multiply signed

norms  = 0660444		" normalize signed
gsm    = 0664000		" get sign and magnitude
lrss   = 0660500		" long right shift signed
llss   = 0660600		" long left shift signed
alss   = 0660700		" AC left shift signed

" Operate Instructions

" Group 1 (OPR 1) instructions
opr    = 0740000		" base operate instruction (nop)
nop    = 0740000
cma    = 0740001		" complement accumulator
cml    = 0740002		" complement link
oas    = 0740004		" inclusive or accumulator switches
ral    = 0740010		" rotate (ac link) left
rar    = 0740020		" rotate (ac link) right
hlt    = 0740040		" HALT
xx     = 0740040
sma    = 0740100		" skip on minus accumulator
sza    = 0740200		" skip on zero accumulator
snl    = 0740400		" skip on non-zero link

skp    = 0741000		" unconditional skip
spa    = 0741100		" skip on positive accumulator
sna    = 0741200		" skip on negative accumulator
szl    = 0741400		" skip on zero link

rtl    = 0742010		" rotate two left (ral*2)
rtr    = 0742020		" rotate two right (rar*2)

cll    = 0744000		" clear link
stl    = 0744002		" set link
rcl    = 0744010		" clear link rotate left
rcr    = 0744020		" clear link rotate right

cla    = 0750000		" clear accumulator
clc    = 0750001		" clear and complement acc
las    = 0750004		" load acc from switches
glk    = 0750010		" get link

" Group 2 operate
law    = 0760000		" load accumulator with (instruction)
