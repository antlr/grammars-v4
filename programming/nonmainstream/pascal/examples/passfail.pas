
Program PassOrFailSystem;

Var

ActualMark : Integer;

PossibleMark : Integer;

PercentageMark : Real;

Begin { PassOrFailSystem }

Writeln ('Please type the student''s actual mark: ');
Readln (ActualMark);

Writeln ('Please type the total possible mark of the exam : ');
Readln (PossibleMark);

PercentageMark := (ActualMark / PossibleMark) * 100;

If (PercentageMark >= 50) Then
Begin
Writeln;
Writeln ('Pass');
End
Else
Begin
Writeln; 
Writeln ('Fail');
End;
{ EndIf }

End. { PassOrFailSystem }
