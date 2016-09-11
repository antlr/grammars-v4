// Generated from com/khubla/ebnf/ebnf.g4 by ANTLR 4.2.2
package com.khubla.ebnf;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link ebnfParser}.
 */
public interface ebnfListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link ebnfParser#rulelist}.
	 * @param ctx the parse tree
	 */
	void enterRulelist(@NotNull ebnfParser.RulelistContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#rulelist}.
	 * @param ctx the parse tree
	 */
	void exitRulelist(@NotNull ebnfParser.RulelistContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#alternation}.
	 * @param ctx the parse tree
	 */
	void enterAlternation(@NotNull ebnfParser.AlternationContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#alternation}.
	 * @param ctx the parse tree
	 */
	void exitAlternation(@NotNull ebnfParser.AlternationContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#oneormore}.
	 * @param ctx the parse tree
	 */
	void enterOneormore(@NotNull ebnfParser.OneormoreContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#oneormore}.
	 * @param ctx the parse tree
	 */
	void exitOneormore(@NotNull ebnfParser.OneormoreContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#rulename}.
	 * @param ctx the parse tree
	 */
	void enterRulename(@NotNull ebnfParser.RulenameContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#rulename}.
	 * @param ctx the parse tree
	 */
	void exitRulename(@NotNull ebnfParser.RulenameContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#optional}.
	 * @param ctx the parse tree
	 */
	void enterOptional(@NotNull ebnfParser.OptionalContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#optional}.
	 * @param ctx the parse tree
	 */
	void exitOptional(@NotNull ebnfParser.OptionalContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#id}.
	 * @param ctx the parse tree
	 */
	void enterId(@NotNull ebnfParser.IdContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#id}.
	 * @param ctx the parse tree
	 */
	void exitId(@NotNull ebnfParser.IdContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#rhs}.
	 * @param ctx the parse tree
	 */
	void enterRhs(@NotNull ebnfParser.RhsContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#rhs}.
	 * @param ctx the parse tree
	 */
	void exitRhs(@NotNull ebnfParser.RhsContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#stringliteral}.
	 * @param ctx the parse tree
	 */
	void enterStringliteral(@NotNull ebnfParser.StringliteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#stringliteral}.
	 * @param ctx the parse tree
	 */
	void exitStringliteral(@NotNull ebnfParser.StringliteralContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#zeroormore}.
	 * @param ctx the parse tree
	 */
	void enterZeroormore(@NotNull ebnfParser.ZeroormoreContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#zeroormore}.
	 * @param ctx the parse tree
	 */
	void exitZeroormore(@NotNull ebnfParser.ZeroormoreContext ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#rule_}.
	 * @param ctx the parse tree
	 */
	void enterRule_(@NotNull ebnfParser.Rule_Context ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#rule_}.
	 * @param ctx the parse tree
	 */
	void exitRule_(@NotNull ebnfParser.Rule_Context ctx);

	/**
	 * Enter a parse tree produced by {@link ebnfParser#element}.
	 * @param ctx the parse tree
	 */
	void enterElement(@NotNull ebnfParser.ElementContext ctx);
	/**
	 * Exit a parse tree produced by {@link ebnfParser#element}.
	 * @param ctx the parse tree
	 */
	void exitElement(@NotNull ebnfParser.ElementContext ctx);
}