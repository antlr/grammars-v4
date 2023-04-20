using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class BencodingLexerBase : Lexer
{
  private int _stringLength = 0;

  public BencodingLexerBase(ICharStream input)
    : base(input)
  {
  }

  public BencodingLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
    : base(input, output, errorOutput)
  {
  }

  public void setStringLength() => this._stringLength = int.Parse(this.Text[..^1]);

  public bool consumeStringChars() => (--this._stringLength) > -1;
}
