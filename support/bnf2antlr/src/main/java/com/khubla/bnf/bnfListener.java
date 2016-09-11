// Generated from com/khubla/bnf/bnf.g4 by ANTLR 4.2.2
package com.khubla.bnf;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link bnfParser}.
 */
public interface bnfListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link bnfParser#alternative}.
	 * @param ctx the parse tree
	 */
	void enterAlternative(@NotNull bnfParser.AlternativeContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#alternative}.
	 * @param ctx the parse tree
	 */
	void exitAlternative(@NotNull bnfParser.AlternativeContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#optional}.
	 * @param ctx the parse tree
	 */
	void enterOptional(@NotNull bnfParser.OptionalContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#optional}.
	 * @param ctx the parse tree
	 */
	void exitOptional(@NotNull bnfParser.OptionalContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#rulelist}.
	 * @param ctx the parse tree
	 */
	void enterRulelist(@NotNull bnfParser.RulelistContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#rulelist}.
	 * @param ctx the parse tree
	 */
	void exitRulelist(@NotNull bnfParser.RulelistContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#lhs}.
	 * @param ctx the parse tree
	 */
	void enterLhs(@NotNull bnfParser.LhsContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#lhs}.
	 * @param ctx the parse tree
	 */
	void exitLhs(@NotNull bnfParser.LhsContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#oneormore}.
	 * @param ctx the parse tree
	 */
	void enterOneormore(@NotNull bnfParser.OneormoreContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#oneormore}.
	 * @param ctx the parse tree
	 */
	void exitOneormore(@NotNull bnfParser.OneormoreContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#alternatives}.
	 * @param ctx the parse tree
	 */
	void enterAlternatives(@NotNull bnfParser.AlternativesContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#alternatives}.
	 * @param ctx the parse tree
	 */
	void exitAlternatives(@NotNull bnfParser.AlternativesContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#text}.
	 * @param ctx the parse tree
	 */
	void enterText(@NotNull bnfParser.TextContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#text}.
	 * @param ctx the parse tree
	 */
	void exitText(@NotNull bnfParser.TextContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#id}.
	 * @param ctx the parse tree
	 */
	void enterId(@NotNull bnfParser.IdContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#id}.
	 * @param ctx the parse tree
	 */
	void exitId(@NotNull bnfParser.IdContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#rhs}.
	 * @param ctx the parse tree
	 */
	void enterRhs(@NotNull bnfParser.RhsContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#rhs}.
	 * @param ctx the parse tree
	 */
	void exitRhs(@NotNull bnfParser.RhsContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#ruleid}.
	 * @param ctx the parse tree
	 */
	void enterRuleid(@NotNull bnfParser.RuleidContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#ruleid}.
	 * @param ctx the parse tree
	 */
	void exitRuleid(@NotNull bnfParser.RuleidContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#zeroormore}.
	 * @param ctx the parse tree
	 */
	void enterZeroormore(@NotNull bnfParser.ZeroormoreContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#zeroormore}.
	 * @param ctx the parse tree
	 */
	void exitZeroormore(@NotNull bnfParser.ZeroormoreContext ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#rule_}.
	 * @param ctx the parse tree
	 */
	void enterRule_(@NotNull bnfParser.Rule_Context ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#rule_}.
	 * @param ctx the parse tree
	 */
	void exitRule_(@NotNull bnfParser.Rule_Context ctx);

	/**
	 * Enter a parse tree produced by {@link bnfParser#element}.
	 * @param ctx the parse tree
	 */
	void enterElement(@NotNull bnfParser.ElementContext ctx);
	/**
	 * Exit a parse tree produced by {@link bnfParser#element}.
	 * @param ctx the parse tree
	 */
	void exitElement(@NotNull bnfParser.ElementContext ctx);
}