MODULE Main;

IMPORT IO;       (* So we can print things *)

VAR name: TEXT;  (* a string variable called "name" *)

BEGIN
  IO.Put("Enter your name: ");
  name:=IO.GetLine();
  IO.Put("Your name is: " & name & "\n");
END Main.
