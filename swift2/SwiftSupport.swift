import Foundation
import Antlr4

fileprivate let TokenEOF = -1

public final class SwiftSupport {
    /* TODO
    There is one caveat to the rules above. If the ! or ? predefined operator
     has no whitespace on the left, it is treated as a postfix operator,
     regardless of whether it has whitespace on the right. To use the ? as
     the optional-chaining operator, it must not have whitespace on the left.
      To use it in the ternary conditional (? :) operator, it must have
      whitespace around both sides.
    */

    /*
    operator-head : /  =  -  +  !  *  %  <  >  &  |  ^  ~  ?
      | [\u00A1-\u00A7]
      | [\u00A9\u00AB]
      | [\u00AC\u00AE]
      | [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
      | [\u2016-\u2017\u2020-\u2027]
      | [\u2030-\u203E]
      | [\u2041-\u2053]
      | [\u2055-\u205E]
      | [\u2190-\u23FF]
      | [\u2500-\u2775]
      | [\u2794-\u2BFF]
      | [\u2E00-\u2E7F]
      | [\u3001-\u3003]
      | [\u3008-\u3030]
      
     */
    private static let operatorHead: Set<Int> = {
        var out = Set<Int>()
        out.insert(SwiftParser.Tokens.BANG.rawValue)
        out.insert(SwiftParser.Tokens.LT.rawValue)
        out.insert(SwiftParser.Tokens.GT.rawValue)
        out.insert(SwiftParser.Tokens.AND.rawValue)
        out.insert(SwiftParser.Tokens.OR.rawValue)
        out.insert(SwiftParser.Tokens.SUB.rawValue)
        out.insert(SwiftParser.Tokens.ADD.rawValue)
        out.insert(SwiftParser.Tokens.MUL.rawValue)
        out.insert(SwiftParser.Tokens.DIV.rawValue)
        out.insert(SwiftParser.Tokens.MOD.rawValue)
        out.insert(SwiftParser.Tokens.EQUAL.rawValue)
        out.insert(SwiftParser.Tokens.CARET.rawValue)
        out.insert(SwiftParser.Tokens.TILDE.rawValue)
        out.insert(SwiftParser.Tokens.QUESTION.rawValue)
        out.formUnion(0xA1 ... 0xA7)
        out.formUnion(0xA9 ... 0xAB)
        out.formUnion(0xAC ... 0xAE)
        out.formUnion(0xB0 ... 0xB1)
        out.insert(0xB6)
        out.insert(0xBB)
        out.insert(0xBF)
        out.insert(0xD7)
        out.insert(0xF7)
        out.formUnion(0x2016 ... 0x2017)
        out.formUnion(0x2020 ... 0x2027)
        out.formUnion(0x2030 ... 0x203E)
        out.formUnion(0x2041 ... 0x2053)
        out.formUnion(0x2055 ... 0x205E)
        out.formUnion(0x2190 ... 0x23FF)
        out.formUnion(0x2500 ... 0x2775)
        out.formUnion(0x2794 ... 0x2BFF)
        out.formUnion(0x2E00 ... 0x2E7F)
        out.formUnion(0x3001 ... 0x3003)
        out.formUnion(0x3008 ... 0x3030)
        return out
    }()

    private static let leftWS: Set<Int> = {
        var out = Set<Int>()
        out.insert(SwiftParser.Tokens.WS.rawValue)
        out.insert(SwiftParser.Tokens.LPAREN.rawValue)
        out.insert(SwiftParser.Tokens.LBRACK.rawValue)
        out.insert(SwiftParser.Tokens.LCURLY.rawValue)
        out.insert(SwiftParser.Tokens.COMMA.rawValue)
        out.insert(SwiftParser.Tokens.COLON.rawValue)
        out.insert(SwiftParser.Tokens.SEMI.rawValue)
        return out
    }()
    
    private static let rightWS: Set<Int> = {
        var out = Set<Int>()
        out.insert(SwiftParser.Tokens.WS.rawValue)
        out.insert(SwiftParser.Tokens.RPAREN.rawValue)
        out.insert(SwiftParser.Tokens.RBRACK.rawValue)
        out.insert(SwiftParser.Tokens.RCURLY.rawValue)
        out.insert(SwiftParser.Tokens.COMMA.rawValue)
        out.insert(SwiftParser.Tokens.COLON.rawValue)
        out.insert(SwiftParser.Tokens.SEMI.rawValue)
        out.insert(SwiftParser.Tokens.Line_comment.rawValue)
        out.insert(SwiftParser.Tokens.Block_comment.rawValue)
        return out
    }()

    private static func isOperatorHead(_ ttype: Int) -> Bool {
        return operatorHead.contains(ttype)
    }

    /*
    Operator_character
      : Operator_head
      | [\u0300-\u036F]
      | [\u1DC0-\u1DFF]
      | [\u20D0-\u20FF]
      | [\uFE00-\uFE0F]
      | [\uFE20-\uFE2F]
      //| [\uE0100-\uE01EF]  ANTLR can't do >16bit char
      
     */
    private static func isOperatorChar(_ ttype: Int) -> Bool {
        return
            operatorHead.contains(ttype) ||
            ttype>=0x0300 && ttype<=0x036F ||
            ttype>=0x1DC0 && ttype<=0x1DFF ||
            ttype>=0x20D0 && ttype<=0x20FF ||
            ttype>=0xFE00 && ttype<=0xFE0F ||
            ttype>=0xFE20 && ttype<=0xFE2F
    }

    /** Find stop token index of next operator return -1 if not operator. */
    private static func getLastOpTokenIndex(_ tokens: TokenStream) throws -> Int {
        var i = tokens.index() // current on-channel lookahead token index
        var lt = try tokens.get(i)
        if try! lt.getType() == SwiftParser.Tokens.DOT.rawValue &&
            tokens.get(i+1).getType()==SwiftParser.Tokens.DOT.rawValue
        {
            // dot-operator
            i+=2 // point at token after ".."
            lt = try tokens.get(i)
            while lt.getType() != TokenEOF &&
                  (lt.getType() == SwiftParser.Tokens.DOT.rawValue || isOperatorChar(lt.getType()))
            {
                i += 1
                lt = try tokens.get(i)
            }
            return i-1
        }
        // Is it regular operator?
        if ( !isOperatorHead(lt.getType()) ) {
            return -1
        }
        i += 1
        lt = try tokens.get(i)
        while lt.getType() != TokenEOF && isOperatorChar(lt.getType()) {
            i += 1
            lt = try tokens.get(i)
        }
        return i-1
    }

    /**
     "If an operator has whitespace around both sides or around neither side,
     it is treated as a binary operator. As an example, the + operator in a+b
     and a + b is treated as a binary operator."
     */
    public static func isBinaryOp(_ tokens: TokenStream) -> Bool {
        let stop = try! getLastOpTokenIndex(tokens)
        guard stop != -1 else { return false }

        let start = tokens.index()
        let prevToken = try! tokens.get(start-1) // includes hidden-channel tokens
        let nextToken = try! tokens.get(stop+1)
        let prevIsWS = isLeftOperatorWS(prevToken)
        let nextIsWS = isRightOperatorWS(nextToken)
        let result = prevIsWS && nextIsWS || (!prevIsWS && !nextIsWS)
        let _ /* text */ = try! tokens.getText(Interval.of(start, stop))
        //System.out.println("isBinaryOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result)
        return result
    }

    /**
     "If an operator has whitespace on the left side only, it is treated as a
     prefix unary operator. As an example, the ++ operator in a ++b is treated
     as a prefix unary operator."
    */
    public static func isPrefixOp(_ tokens: TokenStream) -> Bool {
        let stop = try! getLastOpTokenIndex(tokens)
        guard stop != -1 else { return false }

        let start = tokens.index()
        let prevToken = try! tokens.get(start-1) // includes hidden-channel tokens
        let nextToken = try! tokens.get(stop+1)
        let prevIsWS = isLeftOperatorWS(prevToken)
        let nextIsWS = isRightOperatorWS(nextToken)
        let result = prevIsWS && !nextIsWS
        let _ /* text */ = try! tokens.getText(Interval.of(start, stop))
        //System.out.println("isPrefixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result)
        return result
    }

    /**
     "If an operator has whitespace on the right side only, it is treated as a
     postfix unary operator. As an example, the ++ operator in a++ b is treated
     as a postfix unary operator."

     "If an operator has no whitespace on the left but is followed immediately
     by a dot (.), it is treated as a postfix unary operator. As an example,
     the ++ operator in a++.b is treated as a postfix unary operator (a++ .b
     rather than a ++ .b)."
     */
    public static func isPostfixOp(_ tokens: TokenStream) -> Bool {
        let stop = try! getLastOpTokenIndex(tokens)
        guard stop != -1 else { return false }

        let start = tokens.index()
        let prevToken = try! tokens.get(start-1) // includes hidden-channel tokens
        let nextToken = try! tokens.get(stop+1)
        let prevIsWS = isLeftOperatorWS(prevToken)
        let nextIsWS = isRightOperatorWS(nextToken)
        let result =
            !prevIsWS && nextIsWS ||
            !prevIsWS && nextToken.getType() == SwiftParser.Tokens.DOT.rawValue
        let _ /* text */ = try! tokens.getText(Interval.of(start, stop))
        //System.out.println("isPostfixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result)
        return result
    }

    public static func isOperator(_ tokens: TokenStream, _ op: String) -> Bool {
        let stop = try! getLastOpTokenIndex(tokens)
        guard stop != -1 else { return false }

        let start = tokens.index()
        let text = try! tokens.getText(Interval.of(start, stop))
        return text == op
    }

    private static func isLeftOperatorWS(_ t: Token) -> Bool {
        return leftWS.contains(t.getType())
    }

    private static func isRightOperatorWS(_ t: Token) -> Bool {
        return rightWS.contains(t.getType()) || t.getType() == TokenEOF
    }
}
