/*
BSD License

Copyright (c) 2020, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar rapid;

Data := Value
AccSet Acc Ramp ActUnit MecUnit Add Name AddValue Break
CallBy Var Name Number Clear Name
   Overview
3-45
ClkReset ClkStart
ClkStop
Close IODevice ! Comment
ConfJ [\On] | [\Off]
ConfL [\On] | [\Off] CONNECT Interrupt WITH CorrCon Descr
CorrDiscon Descr
CorrWrite Descr Data CorrClear
DeactUnit MecUnit
Decr Name
EOffsSet EAxOffs

ErrWrite [ \W ] Header Reason [ \RL2]
Exit
ExitCycle
FOR Loop counter FROM Start value [STEP Step value] DO ... ENDFOR
GOTO Label
GripLoad Load
GetSysData DestObject [\ ObjectName ] IDelete Interrupt
IF Condition ...
IF Condition THEN ... {ELSEIF Condition THEN ...}
[ \RL3] [ \RL4]
TO End value
[ELSE ...] ENDIF
Incr Name IndAMove
[ \Ramp ]
IndCMove IndDMove
MecUnit Axis [ \ToAbsPos ] | [ \ToAbsNum ] Speed
MecUnit Axis Speed [ \Ramp ]
MecUnit Axis Delta Speed [ \Ramp ]
IndReset MecUnit Axis [ \RefPos ] | [ \RefNum ] | [ \Short ] | [ \Fwd ] | [ \Bwd ] | [ \Old ]
IndRMove MecUnit Axis [ \ToRelPos ] | [ \ToRelNum ] | [ \Short ] | [ \Fwd ] | [ \Bwd ] Speed [ \Ramp ]
InvertDO Signal
IODisable UnitName MaxTime
IOEnable ISignalDI ISignalDO
UnitName MaxTime
[ \Single ] Signal TriggValue Interrupt
[ \Single ] Signal TriggValue Interrupt

ISleep
ITimer
Interrupt
[ \Single ] Time Interrupt
IVarValue VarNo Value, Interrupt
IWatch Interrupt
Label:
MoveAbsJ [ \Conc ] ToJointPos Speed [ \V ] | [ \T ] Zone [ \Z]
Tool [ \WObj ]
MoveC [ \Conc ] CirPoint ToPoint Speed [ \V ] | [ \T ] Zone [ \Z] Tool [ \WObj ]
MoveCDO CirPoint ToPoint Speed [ \T ] Zone Tool [\WObj ] Signal Value
MoveCSync CirPoint ToPoint Speed [ \T ] Zone Tool [\WObj ] ProcName
MoveJ [\Conc] ToPoint Speed [\V]|[\T] Zone [\Z] Tool [ \WObj ]
MoveJDO ToPoint Speed [ \T ] Zone Tool [ \WObj ] Signal Value
MoveJSync ToPoint Speed [ \T ] Zone Tool [ \WObj ] ProcName
MoveL [\Conc] ToPoint Speed [\V]|[\T] Zone [\Z] Tool [ \WObj ]
MoveLDO ToPoint Speed [ \T ] Zone Tool [ \WObj ] Signal Value
MoveLSync ToPoint Speed [ \T ] Zone Tool [ \WObj ] ProcName
Open Object [\File] IODevice [\Read] | [\Write] | [\Append] | [\Bin] PathResol Value
PDispOn [ \Rot ] [ \ExeP ] ProgPoint Tool [ \WObj ]
PDispSet DispFrame

Procedure
PulseDO
RAISE
{ Argument }
[ \PLength ] Signal
[ Error no ]
Reset Signal
RETURN [ Return value ]
Rewind IODevice
SearchC [ \Stop ] | [ \PStop ] | [ \Sup ] Signal SearchPoint CirPoint
ToPointSpeed [\V]|[\T] Tool [\WObj]
SearchL [ \Stop ] | [ \PStop ] | [ \Sup ] Signal SearchPoint ToPoint Speed [ \V ] | [ \T ] Tool [ \WObj ]
Set Signal
SetAO
SetDO
SetGO
SingArea
SoftAct Axis Softness [\Ramp ] Stop [ \NoRegain ]
Signal Value
[ \SDelay ] Signal Value Signal Value
[ \Wrist] | [ \Arm] | [ \Off]
TEST Test data {CASE Test value {, Test value} : ...} [ DEFAULT: ...] ENDTEST
TPReadFK Answer String FK1 FK2 FK3 FK4 FK5 [\MaxTime] [\DIBreak] [\BreakFlag]
TPReadNum Answer String [\MaxTime] [\DIBreak] [\BreakFlag] TPShow Window
TPWrite String [\Num] | [\Bool] | [\Pos] | [\Orient]
TriggC CirPoint ToPoint Speed [ \T ] Trigg_1 [ \T2 ] [ \T3 ]
[ \T4] Zone Tool [ \WObj ]
TriggInt TriggData Distance [ \Start ] | [ \Time ] Interrupt
TriggIO TriggData Distance [ \Start ] | [ \Time ] [ \DOp ] | [ \GOp ] |
RAPID Summary Syntax Summary
[\AOp ] SetValue [ \DODelay ] | [ \AORamp ]
TriggJ ToPoint Speed [ \T ] Trigg_1 [ \T2 ] Zone Tool [ \WObj ]
TriggL ToPoint Speed [ \T ] Trigg_1 [ \T2 ] Zone Tool [ \WObj ]
TuneServo MecUnit Axis TuneValue TuneServo MecUnit Axis TuneValue [\Type]
[ \T3 ]
[ \T3 ]
[ \T4 ]
[ \T4 ]
 Overview
3-49
UnLoad
VelSet
WaitDI
WaitDO
FilePath [\File] Override Max
WaitTime
WaitUntil
WHILE
[\InPos] Time
[\InPos] Cond [\MaxTime] [\TimeFlag] Condition DO ... ENDWHILE
Signal Value [\MaxTime] [\TimeFlag] Signal Value [\MaxTime] [\TimeFlag]
Write IODevice String [\Num] | [\Bool] | [\Pos] | [\Orient]
[\NoNewLine]
WriteBin IODevice Buffer NChar
WriteStrBin IODevice Str
WZBoxDef [\Inside] | [\Outside] Shape LowPoint HighPoint 1
WZCylDef [\Inside] | [\Outside] Shape CentrePoint Radius Height
WZDisable WorldZone
WZDOSet [\Temp] | [\Stat] WorldZone [\Inside] | [\Before] Shape Signal SetValue
WZEnable WorldZone WZFree WorldZone
WZLimSup WZSphDef
[\Temp] | [\Stat] WorldZone Shape
[\Inside] | [\Outside] Shape CentrePoint Radius

Syntax Summary
RAPID Summary
  19.2 Functions
Abs (Input)
ACos (Value)
AOutput (Signal)
ArgName (Parameter)
ASin (Value)
ATan (Value)
ATan2 (Y X)
ByteToStr (ByteData [\Hex] | [\Okt] | [\Bin] | [\Char])
ClkRead (Clock)
CorrRead
Cos (Angle)
CPos ([Tool] [\WObj])
CRobT ([Tool] [\WObj])
DefDFrame (OldP1 OldP2 OldP3 NewP1 NewP2 NewP3) DefFrame (NewP1 NewP2 NewP3 [\Origin])
Dim (ArrPar DimNo)
DOutput (Signal)
DotProd (Vector1 Vector2)
EulerZYX ([\X] | [\Y] | [\Z] Rotation)
Exp (Exponent)
FileTime ( Path [\ModifyTime] | [\AccessTime] | [\StatCTime] ) GOutput (Signal)

GetTime IndInpos
( [\WDay] | [\Hour] | [\Min] | [\Sec] ) MecUnit Axis


IndSpeed MecUnit Axis [\InSpeed] | [\ZeroSpeed] IsPers (DatObj)
IsVar (DatObj)
MirPos (Point MirPlane [\WObj] [\MirY])
ModTime ( Object ) NOrient (Rotation)
NumToStr (Val Dec [\Exp])
Offs (Point XOffset YOffset ZOffset)
OrientZYX (ZAngle YAngle XAngle)
ORobT (OrgPoint [\InPDisp] | [\InEOffs])
PoseInv (Pose)
PoseMult (Pose1 Pose2)
PoseVect (Pose Pos)
Pow (Base Exponent)
Present (OptPar)
ReadBin (IODevice [\Time])
ReadMotor [\MecUnit ] Axis
ReadNum (IODevice [\Time])
ReadStr (IODevice [\Time])
RelTool (Point Dx Dy Dz [\Rx] [\Ry] [\Rz]) Round ( Val [\Dec])
Sin (Angle)
Sqrt (Value)
StrFind (Str ChPos Set [\NotInSet])
StrLen (Str)

StrMap ( Str FromMap ToMap)
StrMatch (Str ChPos Pattern)
StrMemb (Str ChPos Set)
StrOrder ( Str1 Str2 Order)
StrPart (Str ChPos Len)
StrToByte (ConStr [\Hex] | [\Okt] | [\Bin] | [\Char]) StrToVal ( Str Val )
Tan (Angle)
TestDI (Signal) TestAndSet Object Trunc ( Val [\Dec] ) ValToStr ( Val ) VectMagn (Vector)


WS
   : [ \r\n\t] + -> skip
   ;
