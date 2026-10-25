/*
 [The "BSD license"]
 Copyright (c) 2005-2011 Terence Parr
 All rights reserved.

 Grammar conversion to ANTLR v3:
 Copyright (c) 2011 Sam Harwell
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** Read in an ANTLR grammar and build an AST.  Try not to do
 *  any actions, just build the tree.
 *
 *  The phases are:
 *
 *      antlr.g (this file)
 *      assign.types.g
 *      define.g
 *      buildnfa.g
 *      antlr.print.g (optional)
 *      codegen.g
 *
 *  Terence Parr
 *  University of San Francisco
 *  2005
 */

grammar ANTLR;

options
{
    language=Java;
    output=AST;
    ASTLabelType=GrammarAST;
}

tokens
{
    //OPTIONS='options';
    //TOKENS='tokens';
    LEXER='lexer';
    PARSER='parser';
    CATCH='catch';
    FINALLY='finally';
    GRAMMAR='grammar';
    PRIVATE='private';
    PROTECTED='protected';
    PUBLIC='public';
    RETURNS='returns';
    THROWS='throws';
    TREE='tree';

    RULE;
    PREC_RULE;
    RECURSIVE_RULE_REF; // flip recursive RULE_REF to RECURSIVE_RULE_REF in prec rules
    BLOCK;
    OPTIONAL;
    CLOSURE;
    POSITIVE_CLOSURE;
    SYNPRED;
    RANGE;
    CHAR_RANGE;
    EPSILON;
    ALT;
    EOR;
    EOB;
    EOA; // end of alt
    ID;
    ARG;
    ARGLIST;
    RET;
    LEXER_GRAMMAR;
    PARSER_GRAMMAR;
    TREE_GRAMMAR;
    COMBINED_GRAMMAR;
    INITACTION;
    FORCED_ACTION; // {{...}} always exec even during syn preds
    LABEL; // $x used in rewrite rules
    TEMPLATE;
    SCOPE='scope';
    IMPORT='import';
    GATED_SEMPRED; // {p}? =>
    SYN_SEMPRED; // (...) =>   it's a manually-specified synpred converted to sempred
    BACKTRACK_SEMPRED; // auto backtracking mode syn pred converted to sempred
    FRAGMENT='fragment';
    DOT;
    REWRITES;
}

@lexer::header {
package org.antlr.grammar.v3;
import org.antlr.tool.ErrorManager;
import org.antlr.tool.Grammar;
}

@parser::header {
package org.antlr.grammar.v3;
import org.antlr.tool.ErrorManager;
import org.antlr.tool.Grammar;
import org.antlr.tool.GrammarAST;
import org.antlr.misc.IntSet;
import org.antlr.tool.Rule;
}

@lexer::members {
public boolean hasASTOperator = false;
private String fileName;

public String getFileName() {
    return fileName;
}

public void setFileName(String value) {
    fileName = value;
}

@Override
public Token nextToken() {
    Token token = super.nextToken();
    while (token.getType() == STRAY_BRACKET) {
        ErrorManager.syntaxError(
            ErrorManager.MSG_SYNTAX_ERROR,
            null,
            token,
            "antlr: dangling ']'? make sure to escape with \\]",
            null);

        // skip this token
        token = super.nextToken();
    }

    return token;
}
}

@parser::members {
protected String currentRuleName = null;
protected GrammarAST currentBlockAST = null;
protected boolean atTreeRoot; // are we matching a tree root in tree grammar?

public static ANTLRParser createParser(TokenStream input) {
    ANTLRParser parser = new ANTLRParser(input);
    parser.adaptor = new grammar_Adaptor(parser);
    return parser;
}

private static class GrammarASTErrorNode extends GrammarAST {
    public IntStream input;
    public Token start;
    public Token stop;
    public RecognitionException trappedException;

    public GrammarASTErrorNode(TokenStream input, Token start, Token stop, RecognitionException e) {
        super(stop);
        //Console.Out.WriteLine( "start: " + start + ", stop: " + stop );
        if ( stop == null ||
             ( stop.getTokenIndex() < start.getTokenIndex() &&
              stop.getType() != Token.EOF) ) {
            // sometimes resync does not consume a token (when LT(1) is
            // in follow set.  So, stop will be 1 to left to start. adjust.
            // Also handle case where start is the first token and no token
            // is consumed during recovery; LT(-1) will return null.
            stop = start;
        }
        this.input = input;
        this.start = start;
        this.stop = stop;
        this.trappedException = e;
    }

    @Override
    public boolean isNil() { return false; }

    @Override
    public String getText() {
        String badText = null;
        if (start != null) {
            int i = start.getTokenIndex();
            int j = stop.getTokenIndex();
            if (stop.getType() == Token.EOF) {
                j = input.size();
            }
            badText = ((TokenStream)input).toString(i, j);
        } else {
            // people should subclass if they alter the tree type so this
            // next one is for sure correct.
            badText = "<unknown>";
        }
        return badText;
    }

    @Override
    public void setText(String value) { }

    @Override
    public int getType() { return Token.INVALID_TOKEN_TYPE; }

    @Override
    public void setType(int value) { }

    @Override
    public String toString()
    {
        if (trappedException instanceof MissingTokenException)
        {
            return "<missing type: " +
                   ( (MissingTokenException)trappedException ).getMissingType() +
                   ">";
        } else if (trappedException instanceof UnwantedTokenException) {
            return "<extraneous: " +
                   ( (UnwantedTokenException)trappedException ).getUnexpectedToken() +
                   ", resync=" + getText() + ">";
        } else if (trappedException instanceof MismatchedTokenException) {
            return "<mismatched token: " + trappedException.token + ", resync=" + getText() + ">";
        } else if (trappedException instanceof NoViableAltException) {
            return "<unexpected: " + trappedException.token +
                   ", resync=" + getText() + ">";
        }
        return "<error: " + getText() + ">";
    }
}

static class grammar_Adaptor extends CommonTreeAdaptor {
    ANTLRParser _outer;

    public grammar_Adaptor(ANTLRParser outer) {
        _outer = outer;
    }

    @Override
    public Object create(Token payload) {
        GrammarAST t = new GrammarAST( payload );
        if (_outer != null)
            t.enclosingRuleName = _outer.currentRuleName;
        return t;
    }

    @Override
    public Object errorNode(TokenStream input, Token start, Token stop, RecognitionException e) {
        GrammarAST t = new GrammarASTErrorNode(input, start, stop, e);
        if (_outer != null)
            t.enclosingRuleName = _outer.currentRuleName;
        return t;
    }
}

private Grammar grammar;
private int grammarType;
private String fileName;

public Grammar getGrammar() {
    return grammar;
}

public void setGrammar(Grammar value) {
    grammar = value;
}

public int getGrammarType() {
    return grammarType;
}

public void setGrammarType(int value) {
    grammarType = value;
}

public String getFileName() {
    return fileName;
}

public void setFileName(String value) {
    fileName = value;
}

private final int LA(int i) { return input.LA( i ); }

private final Token LT(int k) { return input.LT( k ); }

/*partial void createTreeAdaptor(ref ITreeAdaptor adaptor)
{
    adaptor = new grammar_Adaptor(this);
}*/

protected GrammarAST setToBlockWithSet(GrammarAST b) {
    /*
     * alt = ^(ALT["ALT"] {b} EOA["EOA"])
     * prefixWithSynpred( alt )
     * return ^(BLOCK["BLOCK"] {alt} EOB["<end-of-block>"])
     */
    GrammarAST alt = (GrammarAST)adaptor.create(ALT, "ALT");
    adaptor.addChild(alt, b);
    adaptor.addChild(alt, adaptor.create(EOA, "<end-of-alt>"));

    prefixWithSynPred(alt);

    GrammarAST block = (GrammarAST)adaptor.create(BLOCK, b.getToken(), "BLOCK");
    adaptor.addChild(block, alt);
    adaptor.addChild(alt, adaptor.create(EOB, "<end-of-block>"));

    return block;
}

/** Create a copy of the alt and make it into a BLOCK; all actions,
 *  labels, tree operators, rewrites are removed.
 */
protected GrammarAST createBlockFromDupAlt(GrammarAST alt) {
    /*
     * ^(BLOCK["BLOCK"] {GrammarAST.dupTreeNoActions(alt)} EOB["<end-of-block>"])
     */
    GrammarAST nalt = GrammarAST.dupTreeNoActions(alt, null);

    GrammarAST block = (GrammarAST)adaptor.create(BLOCK, alt.getToken(), "BLOCK");
    adaptor.addChild( block, nalt );
    adaptor.addChild( block, adaptor.create( EOB, "<end-of-block>" ) );

    return block;
}

/** Rewrite alt to have a synpred as first element;
 *  (xxx)=&gt;xxx
 *  but only if they didn't specify one manually.
 */
protected void prefixWithSynPred( GrammarAST alt ) {
    // if they want backtracking and it's not a lexer rule in combined grammar
    String autoBacktrack = (String)grammar.getBlockOption( currentBlockAST, "backtrack" );
    if ( autoBacktrack == null )
    {
        autoBacktrack = (String)grammar.getOption( "backtrack" );
    }
    if ( autoBacktrack != null && autoBacktrack.equals( "true" ) &&
         !( grammarType == Grammar.COMBINED &&
         Rule.getRuleType(currentRuleName) == Grammar.LEXER) &&
         alt.getChild( 0 ).getType() != SYN_SEMPRED )
    {
        // duplicate alt and make a synpred block around that dup'd alt
        GrammarAST synpredBlockAST = createBlockFromDupAlt( alt );

        // Create a BACKTRACK_SEMPRED node as if user had typed this in
        // Effectively we replace (xxx)=>xxx with {synpredxxx}? xxx
        GrammarAST synpredAST = createSynSemPredFromBlock( synpredBlockAST,
                                                          BACKTRACK_SEMPRED );

        // insert BACKTRACK_SEMPRED as first element of alt
        //synpredAST.getLastSibling().setNextSibling( alt.getFirstChild() );
        //synpredAST.addChild( alt.getFirstChild() );
        //alt.setFirstChild( synpredAST );
        GrammarAST[] children = alt.getChildrenAsArray();
        adaptor.setChild( alt, 0, synpredAST );
        for ( int i = 0; i < children.length; i++ )
        {
            if ( i < children.length - 1 )
                adaptor.setChild( alt, i + 1, children[i] );
            else
                adaptor.addChild( alt, children[i] );
        }
    }
}

protected GrammarAST createSynSemPredFromBlock( GrammarAST synpredBlockAST, int synpredTokenType ) {
    // add grammar fragment to a list so we can make fake rules for them later.
    String predName = grammar.defineSyntacticPredicate( synpredBlockAST, currentRuleName );
    // convert (alpha)=> into {synpredN}? where N is some pred count
    // during code gen we convert to function call with templates
    String synpredinvoke = predName;
    GrammarAST p = (GrammarAST)adaptor.create( synpredTokenType, synpredinvoke );
    // track how many decisions have synpreds
    grammar.blocksWithSynPreds.add( currentBlockAST );
    return p;
}

public static GrammarAST createSimpleRuleAST( String name, GrammarAST block, boolean fragment ) {
    TreeAdaptor adaptor = new grammar_Adaptor(null);

    GrammarAST modifier = null;
    if ( fragment )
    {
        modifier = (GrammarAST)adaptor.create( FRAGMENT, "fragment" );
    }

    /*
     * EOBAST = block.getLastChild()
     * ^(RULE[block,"rule"] ID["name"] {modifier} ARG["ARG"] RET["RET"] SCOPE["scope"] {block} EOR[EOBAST,"<end-of-rule>"])
     */
    GrammarAST rule = (GrammarAST)adaptor.create( RULE, block.getToken(), "rule" );

    adaptor.addChild( rule, adaptor.create( ID, name ) );
    if ( modifier != null )
        adaptor.addChild( rule, modifier );
    adaptor.addChild( rule, adaptor.create( ARG, "ARG" ) );
    adaptor.addChild( rule, adaptor.create( RET, "RET" ) );
    adaptor.addChild( rule, adaptor.create( SCOPE, "scope" ) );
    adaptor.addChild( rule, block );
    adaptor.addChild( rule, adaptor.create( EOR, block.getLastChild().getToken(), "<end-of-rule>" ) );

    return rule;
}

@Override
public void reportError(RecognitionException ex)
{
    //Token token = null;
    //try
    //{
    //    token = LT( 1 );
    //}
    //catch ( TokenStreamException tse )
    //{
    //    ErrorManager.internalError( "can't get token???", tse );
    //}
    Token token = ex.token;
    ErrorManager.syntaxError(
        ErrorManager.MSG_SYNTAX_ERROR,
        grammar,
        token,
        "antlr: " + ex.toString(),
        ex );
}

public void cleanup( GrammarAST root )
{
    if ( grammarType == Grammar.LEXER )
    {
        String filter = (String)grammar.getOption( "filter" );
        GrammarAST tokensRuleAST =
            grammar.addArtificialMatchTokensRule(
                root,
                grammar.lexerRuleNamesInCombined,
                grammar.getDelegateNames(),
                filter != null && filter.equals( "true" ) );
    }
}
}

public
grammar_![Grammar g]
@init
{
    this.grammar = g;
    Map<String, Object> opts;
}
@after
{
    cleanup( $tree );
}
    :   //hdr:headerSpec
        ( ACTION )?
        ( cmt=DOC_COMMENT  )?
        gr=grammarType gid=id {grammar.setName($gid.text);} SEMI
        (   optionsSpec {opts = $optionsSpec.opts; grammar.setOptions(opts, $optionsSpec.start);}
        )?
        (ig=delegateGrammars)?
        (ts=tokensSpec)?
        scopes=attrScopes
        (a=actions)?
        r=rules
        EOF
        -> ^($gr $gid $cmt? optionsSpec? $ig? $ts? $scopes? $a? $r)
    ;

grammarType
    :   (   'lexer'  gr='grammar' {grammarType=Grammar.LEXER; grammar.type = Grammar.LEXER;}       // pure lexer
            -> LEXER_GRAMMAR[$gr]
        |   'parser' gr='grammar' {grammarType=Grammar.PARSER; grammar.type = Grammar.PARSER;}     // pure parser
            -> PARSER_GRAMMAR[$gr]
        |   'tree'   gr='grammar' {grammarType=Grammar.TREE_PARSER; grammar.type = Grammar.TREE_PARSER;}  // a tree parser
            -> TREE_GRAMMAR[$gr]
        |            gr='grammar' {grammarType=Grammar.COMBINED; grammar.type = Grammar.COMBINED;} // merged parser/lexer
            -> COMBINED_GRAMMAR[$gr]
        )
    ;

actions
    :   (action)+
    ;

/** Match stuff like @parser::members {int i;} */
action
    :   AMPERSAND^ (actionScopeName COLON! COLON!)? id ACTION
    ;

/** Sometimes the scope names will collide with keywords; allow them as
 *  ids for action scopes.
 */
actionScopeName
    :   id
    |   l='lexer'
        -> ID[$l]
    |   p='parser'
        -> ID[$p]
    ;

optionsSpec returns [Map<String, Object> opts=new HashMap<String, Object>()]
    :   OPTIONS^ (option[$opts] SEMI!)+ RCURLY!
    ;

option[Map<String, Object> opts]
    :   id ASSIGN^ optionValue
        {
            $opts.put($id.text, $optionValue.value);
        }
    ;

optionValue returns [Object value = null]
    :   x=id             {$value = $x.text;}
    |   s=STRING_LITERAL {String vs = $s.text;
                          // remove the quotes:
                          $value=vs.substring(1,vs.length()-1);}
    |   c=CHAR_LITERAL   {String vs = $c.text;
                          // remove the quotes:
                          $value=vs.substring(1,vs.length()-1);}
    |   i=INT            {$value = Integer.parseInt($i.text);}
    |   ss=STAR          {$value = "*";} // used for k=*
        -> STRING_LITERAL[$ss]
//  |   cs:charSet       {value = #cs;} // return set AST in this case
    ;

delegateGrammars
    :   'import'^ delegateGrammar (COMMA! delegateGrammar)* SEMI!
    ;

delegateGrammar
    :   lab=id ASSIGN^ g=id {grammar.importGrammar($g.tree, $lab.text);}
    |   g2=id               {grammar.importGrammar($g2.tree,null);}
    ;

tokensSpec
    :   TOKENS^
            tokenSpec*
        RCURLY!
    ;

tokenSpec
    :   TOKEN_REF ( ASSIGN^ (STRING_LITERAL|CHAR_LITERAL) )? SEMI!
    ;

attrScopes
    :   (attrScope)*
    ;

attrScope
    :   'scope'^ id ruleActions? ACTION
    ;

rules
    :   (   rule
        )+
    ;

public
rule
@init
{
    GrammarAST eob=null;
    CommonToken start = (CommonToken)LT(1);
    int startLine = LT(1).getLine();
}
    :
    (   (   d=DOC_COMMENT
        )?
        (   p1='protected'  //{modifier=$p1.tree;}
        |   p2='public'     //{modifier=$p2.tree;}
        |   p3='private'    //{modifier=$p3.tree;}
        |   p4='fragment'   //{modifier=$p4.tree;}
        )?
        ruleName=id
        {
            currentRuleName=$ruleName.text;
            if ( grammarType==Grammar.LEXER && $p4==null )
                grammar.lexerRuleNamesInCombined.add(currentRuleName);
        }
        ( BANG )?
        ( aa=ARG_ACTION )?
        ( 'returns' rt=ARG_ACTION  )?
        ( throwsSpec )?
        ( optionsSpec )?
        scopes=ruleScopeSpec
        (ruleActions)?
        COLON
        ruleAltList[$optionsSpec.opts]
        SEMI
        ( ex=exceptionGroup )?
        ->  ^(  RULE[$ruleName.start, "rule"]
                $ruleName
                // the modifier will be 0 or one of the modifiers:
                $p1? $p2? $p3? $p4?
                ^(ARG["ARG"] $aa?)
                ^(RET["RET"] $rt?)
                throwsSpec?
                optionsSpec?
                $scopes
                ruleActions?
                ruleAltList
                $ex?
                EOR[$SEMI,"<end-of-rule>"])
    )
    {
        $tree.setTreeEnclosingRuleNameDeeply(currentRuleName);
        ((GrammarAST)$tree.getChild(0)).setBlockOptions($optionsSpec.opts);
    }
    ;

ruleActions
    :   (ruleAction)+
    ;

/** Match stuff like @init {int i;} */
ruleAction
    :   AMPERSAND^ id ACTION
    ;

throwsSpec
    :   'throws'^ id ( COMMA! id )*
    ;

ruleScopeSpec
    :   ( 'scope' ruleActions? ACTION )?
        ( 'scope' idList SEMI )*
        -> ^(SCOPE[$start,"scope"] ruleActions? ACTION? idList*)
    ;

ruleAltList[Map<String, Object> opts]
@init
{
    GrammarAST blkRoot = null;
    GrammarAST save = currentBlockAST;
}
    :   ( -> BLOCK[input.LT(-1),"BLOCK"] )
        {
            blkRoot = (GrammarAST)$tree.getChild(0);
            blkRoot.setBlockOptions($opts);
            currentBlockAST = blkRoot;
        }
        (   a1=alternative r1=rewrite
            {if (LA(1)==OR||(LA(2)==QUESTION||LA(2)==PLUS||LA(2)==STAR)) prefixWithSynPred($a1.tree);}
            -> $a1 $r1?
        )
        (   (   OR a2=alternative r2=rewrite
                {if (LA(1)==OR||(LA(2)==QUESTION||LA(2)==PLUS||LA(2)==STAR)) prefixWithSynPred($a2.tree);}
                -> $ruleAltList $a2 $r2?
            )+
        |
        )
        -> ^({blkRoot} $ruleAltList EOB["<end-of-block>"])
    ;
finally { currentBlockAST = save; }

/** Build #(BLOCK ( #(ALT ...) EOB )+ ) */
block
@init
{
    GrammarAST save = currentBlockAST;
}
    :   (   lp=LPAREN
            -> BLOCK[$lp,"BLOCK"]
        )
        {currentBlockAST = (GrammarAST)$tree.getChild(0);}
        (
            // 2nd alt and optional branch ambig due to
            // linear approx LL(2) issue.  COLON ACTION
            // matched correctly in 2nd alt.
            (optionsSpec {((GrammarAST)$tree.getChild(0)).setOptions(grammar,$optionsSpec.opts);})?
            ( ruleActions )?
            COLON
        |   ACTION COLON
        )?

        a=alternative r=rewrite
        {
            stream_alternative.add( $r.tree );
            if ( LA(1)==OR || (LA(2)==QUESTION||LA(2)==PLUS||LA(2)==STAR) )
                prefixWithSynPred($a.tree);
        }
        (   OR a=alternative r=rewrite
            {
                stream_alternative.add( $r.tree );
                if (LA(1)==OR||(LA(2)==QUESTION||LA(2)==PLUS||LA(2)==STAR))
                    prefixWithSynPred($a.tree);
            }
        )*

        rp=RPAREN
        -> ^($block optionsSpec? ruleActions? ACTION? alternative+ EOB[$rp,"<end-of-block>"])
    ;
finally { currentBlockAST = save; }

// ALT and EOA have indexes tracking start/stop of entire alt
alternative
    :   element+
        -> ^(ALT[$start,"ALT"] element+ EOA[input.LT(-1),"<end-of-alt>"])
    |   // epsilon alt
        -> ^(ALT[$start,"ALT"] EPSILON[input.LT(-1),"epsilon"] EOA[input.LT(-1),"<end-of-alt>"])
    ;

exceptionGroup
    :   exceptionHandler+ finallyClause?
    |   finallyClause
    ;

exceptionHandler
    :   'catch'^ ARG_ACTION ACTION
    ;

finallyClause
    :   'finally'^ ACTION
    ;

element
    :   elementNoOptionSpec
    ;

elementNoOptionSpec
@init
{
    IntSet elements=null;
}
    :   (   id (ASSIGN^|PLUS_ASSIGN^)
            (   atom (sub=ebnfSuffix[root_0,false]! {root_0 = $sub.tree;})?
            |   ebnf
            )
        |   a=atom
            (   sub2=ebnfSuffix[$a.tree,false]! {root_0=$sub2.tree;}
            )?
        |   ebnf
        |   FORCED_ACTION
        |   ACTION
        |   p=SEMPRED ( IMPLIES! {$p.setType(GATED_SEMPRED);} )?
            {
            grammar.blocksWithSemPreds.add(currentBlockAST);
            }
        |   t3=tree_
        )
    ;

atom
    :   range (ROOT^|BANG^)?
    |   (
            // grammar.rule but ensure no spaces. "A . B" is not a qualified ref
            // We do here rather than lexer so we can build a tree
            ({LT(1).getCharPositionInLine()+LT(1).getText().length()==LT(2).getCharPositionInLine()&&
             LT(2).getCharPositionInLine()+1==LT(3).getCharPositionInLine()}? id WILDCARD (terminal|ruleref)) =>
            id w=WILDCARD^ (terminal|ruleref) {$w.setType(DOT);}
        |   terminal
        |   ruleref
        )
    |   notSet (ROOT^|BANG^)?
    ;

ruleref
    :   RULE_REF^ ARG_ACTION? (ROOT^|BANG^)?
    ;

notSet
    :   NOT^
        (   notTerminal
        |   block
        )
    ;

treeRoot
@init{atTreeRoot=true;}
@after{atTreeRoot=false;}
    :   id (ASSIGN^|PLUS_ASSIGN^) (atom|block)
    |   atom
    |   block
    ;

tree_
    :   TREE_BEGIN^
        treeRoot element+
        RPAREN!
    ;

/** matches ENBF blocks (and sets via block rule) */
ebnf
    :   block
        (   QUESTION
            -> ^(OPTIONAL[$start,"?"] block)
        |   STAR
            -> ^(CLOSURE[$start,"*"] block)
        |   PLUS
            -> ^(POSITIVE_CLOSURE[$start,"+"] block)
        |   IMPLIES // syntactic predicate
            // ignore for lexer rules in combined
            -> {grammarType == Grammar.COMBINED && Rule.getRuleType(currentRuleName) == Grammar.LEXER}? ^(SYNPRED[$start,"=>"] block)
            // create manually specified (...)=> predicate; convert to sempred
            -> {createSynSemPredFromBlock($block.tree, SYN_SEMPRED)}
        |   ROOT
            -> ^(ROOT block)
        |   BANG
            -> ^(BANG block)
        |
            -> block
        )
    ;

range!
    :   {Rule.getRuleType(currentRuleName) == Grammar.LEXER}? =>
        c1=CHAR_LITERAL RANGE c2=CHAR_LITERAL
        -> ^(CHAR_RANGE[$c1,".."] $c1 $c2)
    |   // range elsewhere is an error
        (   t=TOKEN_REF r=RANGE TOKEN_REF
        |   t=STRING_LITERAL r=RANGE STRING_LITERAL
        |   t=CHAR_LITERAL r=RANGE CHAR_LITERAL
        )
        {
        ErrorManager.syntaxError(
            ErrorManager.MSG_RANGE_OP_ILLEGAL,grammar,$r,null,null);
        }
        -> $t // have to generate something for surrounding code, just return first token
    ;

terminal
    :   cl=CHAR_LITERAL^ ( elementOptions[$cl.tree]! )? (ROOT^|BANG^)?

    |   tr=TOKEN_REF^
        ( elementOptions[$tr.tree]! )?
        ( ARG_ACTION )? // Args are only valid for lexer rules
        (ROOT^|BANG^)?

    |   sl=STRING_LITERAL^ ( elementOptions[$sl.tree]! )? (ROOT^|BANG^)?

    |   wi=WILDCARD (ROOT^|BANG^)?
        {
            if ( atTreeRoot )
            {
                ErrorManager.syntaxError(
                    ErrorManager.MSG_WILDCARD_AS_ROOT,grammar,$wi,null,null);
            }
        }
    ;

elementOptions[GrammarAST terminalAST]
    :   OPEN_ELEMENT_OPTION^ defaultNodeOption[terminalAST] CLOSE_ELEMENT_OPTION!
    |   OPEN_ELEMENT_OPTION^ elementOption[terminalAST] (SEMI! elementOption[terminalAST])* CLOSE_ELEMENT_OPTION!
    ;

defaultNodeOption[GrammarAST terminalAST]
    :   elementOptionId
        {terminalAST.setTerminalOption(grammar,Grammar.defaultTokenOption,$elementOptionId.qid);}
    ;

elementOption[GrammarAST terminalAST]
    :   id ASSIGN^
        (   elementOptionId
            {terminalAST.setTerminalOption(grammar,$id.text,$elementOptionId.qid);}
        |   (t=STRING_LITERAL|t=DOUBLE_QUOTE_STRING_LITERAL|t=DOUBLE_ANGLE_STRING_LITERAL)
            {terminalAST.setTerminalOption(grammar,$id.text,$t.text);}
        )
    ;

elementOptionId returns [String qid]
@init{StringBuffer buf = new StringBuffer();}
    :   i=id {buf.append($i.text);} ('.' i=id {buf.append("." + $i.text);})*
        {$qid = buf.toString();}
    ;

ebnfSuffix[GrammarAST elemAST, boolean inRewrite]
@init
{
GrammarAST blkRoot=null;
GrammarAST alt=null;
GrammarAST save = currentBlockAST;
}
@after
{
currentBlockAST = save;
}
    :   (   -> BLOCK[$elemAST.getToken(), "BLOCK"]
        )
        { blkRoot = (GrammarAST)$tree.getChild(0); currentBlockAST = blkRoot; }
        (   // create alt
            -> ^(ALT[$elemAST.getToken(), "ALT"] {$elemAST} EOA["<end-of-alt>"])
        )
        {
            alt = (GrammarAST)$tree.getChild(0);
            if ( !inRewrite )
                prefixWithSynPred(alt);
        }
        (   QUESTION
            -> OPTIONAL[$elemAST.getToken(),"?"]
        |   STAR
            -> CLOSURE[$elemAST.getToken(),"*"]
        |   PLUS
            -> POSITIVE_CLOSURE[$elemAST.getToken(),"+"]
        )
        -> ^($ebnfSuffix ^({blkRoot} {alt} EOB[$elemAST.getToken(), "<end-of-block>"]))
    ;

notTerminal
    :   CHAR_LITERAL
    |   TOKEN_REF
    |   STRING_LITERAL
    ;

idList
    :   id (COMMA! id)*
    ;

id
    :   TOKEN_REF
        -> ID[$TOKEN_REF]
    |   RULE_REF
        -> ID[$RULE_REF]
    ;

// R E W R I T E  S Y N T A X

rewrite
    :   rewrite_with_sempred*
        REWRITE rewrite_alternative
        -> ^(REWRITES rewrite_with_sempred* ^(REWRITE rewrite_alternative))
    |
    ;

rewrite_with_sempred
    :   REWRITE^ SEMPRED rewrite_alternative
    ;

rewrite_block
    :   LPAREN
        rewrite_alternative
        RPAREN
        -> ^(BLOCK[$LPAREN,"BLOCK"] rewrite_alternative EOB[$RPAREN,"<end-of-block>"])
    ;

rewrite_alternative
options{k=1;}
    :   {grammar.buildTemplate()}? => rewrite_template

    |   {grammar.buildAST()}? => ( rewrite_element )+
        -> {!stream_rewrite_element.hasNext()}? ^(ALT[LT(1),"ALT"] EPSILON["epsilon"] EOA["<end-of-alt>"])
        -> ^(ALT[LT(1),"ALT"] rewrite_element+ EOA["<end-of-alt>"])

    |
        -> ^(ALT[LT(1),"ALT"] EPSILON["epsilon"] EOA["<end-of-alt>"])
    |   {grammar.buildAST()}? ETC
    ;

rewrite_element
    :   (   t=rewrite_atom
            -> $t
        )
        (   subrule=ebnfSuffix[$t.tree,true]
            -> $subrule
        )?
    |   rewrite_ebnf
    |   (   tr=rewrite_tree
            -> $tr
        )
        (   subrule=ebnfSuffix[$tr.tree,true]
            -> $subrule
        )?
    ;

rewrite_atom
    :   tr=TOKEN_REF^ elementOptions[$tr.tree]!? ARG_ACTION? // for imaginary nodes
    |   RULE_REF
    |   cl=CHAR_LITERAL elementOptions[$cl.tree]!?
    |   sl=STRING_LITERAL elementOptions[$sl.tree]!?
    |   DOLLAR! label // reference to a label in a rewrite rule
    |   ACTION
    ;

label
    :   TOKEN_REF -> LABEL[$TOKEN_REF]
    |   RULE_REF -> LABEL[$RULE_REF]
    ;

rewrite_ebnf
    :   b=rewrite_block
        (   QUESTION
            -> ^(OPTIONAL[$b.start,"?"] $b)
        |   STAR
            -> ^(CLOSURE[$b.start,"*"] $b)
        |   PLUS
            -> ^(POSITIVE_CLOSURE[$b.start,"+"] $b)
        )
    ;

rewrite_tree
    :   TREE_BEGIN^
            rewrite_atom rewrite_element*
        RPAREN!
    ;

/** Build a tree for a template rewrite:
      ^(TEMPLATE (ID|ACTION) ^(ARGLIST ^(ARG ID ACTION) ...) )
    where ARGLIST is always there even if no args exist.
    ID can be "template" keyword.  If first child is ACTION then it's
    an indirect template ref

    -> foo(a={...}, b={...})
    -> ({string-e})(a={...}, b={...})  // e evaluates to template name
    -> {%{$ID.text}} // create literal template from string (done in ActionTranslator)
    -> {st-expr} // st-expr evaluates to ST
 */
public
rewrite_template
options{k=1;}
    :   // -> template(a={...},...) "..."
        {LT(1).getText().equals("template")}? => // inline
        (   rewrite_template_head
            -> rewrite_template_head
        )
        ( st=DOUBLE_QUOTE_STRING_LITERAL | st=DOUBLE_ANGLE_STRING_LITERAL )
        { adaptor.addChild( $tree.getChild(0), adaptor.create($st) ); }

    |   // -> foo(a={...}, ...)
        rewrite_template_head

    |   // -> ({expr})(a={...}, ...)
        rewrite_indirect_template_head

    |   // -> {...}
        ACTION
    ;

/** -> foo(a={...}, ...) */
rewrite_template_head
    :   id lp=LPAREN
        rewrite_template_args
        RPAREN
        -> ^(TEMPLATE[$lp,"TEMPLATE"] id rewrite_template_args)
    ;

/** -> ({expr})(a={...}, ...) */
rewrite_indirect_template_head
    :   lp=LPAREN
        ACTION
        RPAREN
        LPAREN rewrite_template_args RPAREN
        -> ^(TEMPLATE[$lp,"TEMPLATE"] ACTION rewrite_template_args)
    ;

rewrite_template_args
    :   rewrite_template_arg (COMMA rewrite_template_arg)*
        -> ^(ARGLIST["ARGLIST"] rewrite_template_arg+)
    |
        -> ARGLIST["ARGLIST"]
    ;

rewrite_template_arg
    :   id a=ASSIGN ACTION
        -> ^(ARG[$a,"ARG"] id ACTION)
    ;

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
// L E X E R

// get rid of warnings:
fragment STRING_LITERAL : ;
fragment FORCED_ACTION : ;
fragment DOC_COMMENT : ;
fragment SEMPRED : ;

WS
    :   (   ' '
        |   '\t'
        |   ('\r')? '\n'
        )
        { $channel = HIDDEN; }
    ;

COMMENT
@init{List<Integer> type = new ArrayList<Integer>() {{ add(0); }};}
    :   ( SL_COMMENT | ML_COMMENT[type] {$type = type.get(0);} )
        {
            if ( $type != DOC_COMMENT )
                $channel = HIDDEN;
        }
    ;

fragment
SL_COMMENT
    :   '//'
        (   (' $ANTLR') => ' $ANTLR ' SRC (('\r')? '\n')? // src directive
        |   ~('\r'|'\n')* (('\r')? '\n')?
        )
    ;

fragment
ML_COMMENT[List<Integer> type]
    :   '/*'
        {$type.set(0, (input.LA(1) == '*' && input.LA(2) != '/') ? DOC_COMMENT : ML_COMMENT);}
        .*
        '*/'
    ;

OPEN_ELEMENT_OPTION
    :   '<'
    ;

CLOSE_ELEMENT_OPTION
    :   '>'
    ;

AMPERSAND : '@';

COMMA : ',';

QUESTION :  '?' ;

TREE_BEGIN : '^(' ;

LPAREN: '(' ;

RPAREN: ')' ;

COLON : ':' ;

STAR:   '*' ;

PLUS:   '+' ;

ASSIGN : '=' ;

PLUS_ASSIGN : '+=' ;

IMPLIES : '=>' ;

REWRITE : '->' ;

SEMI:   ';' ;

ROOT : '^' {hasASTOperator=true;} ;

BANG : '!' {hasASTOperator=true;} ;

OR  :   '|' ;

WILDCARD : '.' ;

ETC : '...' ;

RANGE : '..' ;

NOT :   '~' ;

RCURLY: '}' ;

DOLLAR : '$' ;

STRAY_BRACKET
    :   ']'
    ;

CHAR_LITERAL
    :   '\''
        (   ESC
        |   ~('\\'|'\'')
        )*
        '\''
        {
            StringBuffer s = Grammar.getUnescapedStringFromGrammarStringLiteral($text);
            if ( s.length() > 1 )
            {
                $type = STRING_LITERAL;
            }
        }
    ;

DOUBLE_QUOTE_STRING_LITERAL
@init
{
    StringBuilder builder = new StringBuilder();
}
    :   '"'                         {builder.append('"');}
        (   ('\\\"') => '\\' '"'    {builder.append('"');}
        |   '\\' c=~'"'             {builder.append("\\" + (char)$c);}
        |   c=~('\\'|'"')           {builder.append((char)$c);}
        )*
        '"'                         {builder.append('"');}
        {
            setText(builder.toString());
        }
    ;

DOUBLE_ANGLE_STRING_LITERAL
    :   '<<' .* '>>'
    ;

fragment
ESC
    :   '\\'
        (   // due to the way ESC is used, we don't need to handle the following character in different ways
            /*'n'
        |   'r'
        |   't'
        |   'b'
        |   'f'
        |   '"'
        |   '\''
        |   '\\'
        |   '>'
        |   'u' XDIGIT XDIGIT XDIGIT XDIGIT
        |*/ . // unknown, leave as it is
        )
    ;

fragment
DIGIT
    :   '0'..'9'
    ;

fragment
XDIGIT
    :   '0' .. '9'
    |   'a' .. 'f'
    |   'A' .. 'F'
    ;

INT
    :   ('0'..'9')+
    ;

ARG_ACTION
@init {
    List<String> text = new ArrayList<String>() {{ add(null); }};
}
    :   '['
        NESTED_ARG_ACTION[text]
        ']'
        {setText(text.get(0));}
    ;

fragment
NESTED_ARG_ACTION[List<String> text]
@init {
    $text.set(0, "");
    StringBuilder builder = new StringBuilder();
}
    :   (   ('\\]') => '\\' ']'     {builder.append("]");}
        |   '\\' c=~(']')           {builder.append("\\" + (char)$c);}
        |   ACTION_STRING_LITERAL   {builder.append($ACTION_STRING_LITERAL.text);}
        |   ACTION_CHAR_LITERAL     {builder.append($ACTION_CHAR_LITERAL.text);}
        |   c=~('\\'|'"'|'\''|']')  {builder.append((char)$c);}
        )*
        {
            $text.set(0, builder.toString());
        }
    ;

ACTION
@init
{
    int actionLine = getLine();
    int actionColumn = getCharPositionInLine();
}
    :   NESTED_ACTION
        ('?' {$type = SEMPRED;})?
        {
            String action = $text;
            int n = 1; // num delimiter chars
            if ( action.startsWith("{{") && action.endsWith("}}") )
            {
                $type = FORCED_ACTION;
                n = 2;
            }
            action = action.substring(n,action.length()-n - ($type==SEMPRED ? 1 : 0));
            setText(action);
        }
    ;

fragment
NESTED_ACTION
    :   '{'
        (   NESTED_ACTION
        |   ACTION_CHAR_LITERAL
        |   ('//' | '/*') => COMMENT
        |   ACTION_STRING_LITERAL
        |   ACTION_ESC
        |   ~('{'|'\''|'"'|'\\'|'}')
        )*
        '}'
    ;

fragment
ACTION_CHAR_LITERAL
    :   '\''
        (   ACTION_ESC
        |   ~('\\'|'\'')
        )*
        '\''
    ;

fragment
ACTION_STRING_LITERAL
    :   '"'
        (   ACTION_ESC
        |   ~('\\'|'"')
        )*
        '"'
    ;

fragment
ACTION_ESC
    :   '\\\''
    |   '\\\"'
    |   '\\' ~('\''|'"')
    ;

TOKEN_REF
    :   'A'..'Z'
        (   'a'..'z'|'A'..'Z'|'_'|'0'..'9'
        )*
    ;

TOKENS
    :   'tokens' WS_LOOP '{'
    ;

OPTIONS
    :   'options' WS_LOOP '{'
    ;

// we get a warning here when looking for options '{', but it works right
RULE_REF
@init
{
    int t=0;
}
    :   'a'..'z' ('a'..'z' | 'A'..'Z' | '_' | '0'..'9')*
    ;

fragment
WS_LOOP
    :   (   WS
        |   COMMENT
        )*
    ;

fragment
WS_OPT
    :   (WS)?
    ;

/** Reset the file and line information; useful when the grammar
 *  has been generated so that errors are shown relative to the
 *  original file like the old C preprocessor used to do.
 */
fragment
SRC
    :   'src' ' ' file=ACTION_STRING_LITERAL ' ' line=INT
        {
            setFileName($file.text.substring(1,$file.text.length()-1));
            input.setLine(Integer.parseInt($line.text) - 1);  // -1 because SL_COMMENT will increment the line no. KR
        }
    ;