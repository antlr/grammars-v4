(* This is designed to run Under Oberon V4 *)

MODULE Hello;
  IMPORT Oberon, Texts;
  VAR W: Texts.Writer;
  
  PROCEDURE World*;
  BEGIN
    Texts.WriteString(W, "Hello World!");
    Texts.WriteLn(W);
    Texts.Append(Oberon.Log, W.buf);
  END World;

BEGIN
  Texts.OpenWriter(W);
END Hello.
