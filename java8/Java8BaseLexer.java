import org.antlr.v4.runtime.*;

/**
 * All lexer methods that used in grammar (IsStrictMode)
 * should start with Upper Case Char similar to Lexer rules.
 */
public abstract class Java8BaseLexer extends Lexer
{
    /* 
    https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#isJavaIdentifierPart-int-
    A character may be part of a Java identifier if any of the following conditions are true:
    it is a letter                                                      (L)
    it is a currency symbol (such as '$')                               (Sc)
    it is a connecting punctuation character (such as '_')              (Pc)
    it is a digit                                                       (No)?
    it is a numeric letter (such as a Roman numeral character)          (Nl)
    it is a combining mark                                              (Mc)
    it is a non-spacing mark                                            (Mn)
    isIdentifierIgnorable(codePoint) returns true for the code point 
     */

    protected boolean wasJavaIdentiferStart(){
        return Character.isJavaIdentifierStart(_input.LA(-1));
    }

    protected boolean wasJavaIdentiferStartUTF16(){
        return Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)));
    }
    /*
     A character may start a Java identifier if and only if one of the following conditions is true:

    isLetter(codePoint) returns true                                                (L)
    getType(codePoint) returns LETTER_NUMBER                                        (Nl)
    the referenced character is a currency symbol  (such as '$')                    (Sc)
    the referenced character is a connecting punctuation character (such as '_').   (Pc)

    These conditions are tested against the character information from version 6.2 of the Unicode Standard.
    */
    protected boolean wasJavaIdentiferPart(){
        return Character.isJavaIdentifierPart(_input.LA(-1));
    }

    protected boolean wasJavaIdentiferPartUTF16(){
        return Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)));
    }
}