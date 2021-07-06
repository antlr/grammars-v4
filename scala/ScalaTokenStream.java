import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;

import java.util.*;

import static ScalaLexer.*;

public class ScalaTokenStream extends CommonTokenStream {
    private static final Set<Integer> nonStarters =  new HashSet<>(Arrays.asList(Catch, Else, Extends, Finally, ForSome, Match, With,
            Yield, Comma, Dot, Colon, SemiColon, Eq, Arrow, Assign, LowerType, ViewBound, UpperType, Hash, LBracket,
            RParen, RBracket, RBrace, /* per spec */
            DoubleQuoteSingle, TripleDoubleQuoteMulti /* custom cases */
    ));
    private static final Set<Integer> terminators = new HashSet<>(Arrays.asList(This, Null, BooleanLiteral, Return, Type, RParen,
            RBracket, RBrace, UnderScore, IntegerLiteral, FloatingPointLiteral, StringLiteral, CharacterLiteral,
            /* Id equivalents */ AlphaId, VarId, BackTickId,
            /* Tokens that are part of the operator token */ OpChar, Hash, Colon, Or,
            /* Tokens that are part of the operator token */ Exclamation, Plus, Minus, Tilde, Star, ViewBound,
            /* XML Terminators */ XMLCloseTag, XMLAutoClose,
            /* Custom tokens for interpolated strings */ DoubleQuoteSingle, TripleDoubleQuoteMulti
            /*SymbolLiteral was removed*/));
    Stack<Boolean> newLineEnables = new Stack<>();
    int lastIndex = -1;
    private boolean inCase = false;

    public ScalaTokenStream(TokenSource tokenSource) {
        super(tokenSource);
    }

    @Override
    public Token LT(int k) {
        Token t = super.LT(k);
        if (k < 0) {
            return t;
        }

        if (k != 1) {
            while (t.getType() == NL) {
                k++;
                t = super.LT(k);
            }
            return t;
        }

        if (lastIndex != t.getTokenIndex()) {
            if (lastIndex < t.getTokenIndex()) {
                for (int i = lastIndex + 1; i <= t.getTokenIndex(); i++) {
                    this.advance(this.get(i));
                }
            } else {
                for (int i = lastIndex; i > t.getTokenIndex(); i--) {
                    this.stepBack(this.get(i));
                }
            }
            lastIndex = t.getTokenIndex();
        }
        if (t.getType() == NL) {
            boolean canEmitNLToken = canEmitNLToken();
            if (!canEmitNLToken) {
                super.consume();// skip the NL token
                return this.LT(1);
            }
        }
        return t;
    }

    private boolean canEmitNLToken() {
        Token previousToken = super.LB(1);
        Token nextToken = super.LT(2);
        boolean afterStatementTerminator = previousToken != null && terminators.contains(previousToken.getType());

        boolean beforeStatementStarter = nextToken != null && !nonStarters.contains(nextToken.getType())
                || (Objects.requireNonNull(nextToken).getType() == Case
                && (super.LT(3) != null && super.LT(3).getType() == Class || super.LT(3).getType() == Object));

        if(inCase){
            return false;
        }

        return ( isEnabledRegion() && afterStatementTerminator && beforeStatementStarter);
    }

    private Boolean isEnabledRegion() {
        return newLineEnables.isEmpty() || newLineEnables.peek();
    }

    private void advance(Token t) {
        switch (t.getType()) {
            case Case:
                if (super.LT(2).getType() != Class && super.LT(2).getType() != Object ) {
                    newLineEnables.push(true);
                    inCase = true;
                }
                break;
            case Arrow:
                if (inCase) {
                    arrowsForCase.add(t.getTokenIndex());
                    newLineEnables.pop();
                    inCase = false;
                }
                break;
            case LBrace:
                newLineEnables.push(true);
                break;
            case LParen:
            case LBracket:
                newLineEnables.push(false);
                break;
            case RBrace:
            case RParen:
            case RBracket:
                newLineEnables.pop();
                break;
        }
    }

    HashSet<Integer> arrowsForCase = new HashSet<>();

    private void stepBack(Token t) {
        switch (t.getType()) {
            case Case:
                if (inCase) {
                    newLineEnables.pop();
                    inCase = false;
                }
                break;
            case Arrow:
                if (arrowsForCase.contains(t.getTokenIndex())) {
                    newLineEnables.push(false);
                    inCase = true;
                }
                break;
            case RBrace:
                newLineEnables.push(true);
                break;
            case RParen:
            case RBracket:
                newLineEnables.push(false);
                break;
            case LBrace:
            case LParen:
            case LBracket:
                newLineEnables.pop();
                break;
        }
    }
}
