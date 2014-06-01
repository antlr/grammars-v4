// Generated from org/antlr/abnf/Abnf.g4 by ANTLR 4.2.2
package org.antlr.abnf;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link AbnfParser}.
 */
public interface AbnfListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link AbnfParser#element}.
	 * @param ctx the parse tree
	 */
	void enterElement(@NotNull AbnfParser.ElementContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#element}.
	 * @param ctx the parse tree
	 */
	void exitElement(@NotNull AbnfParser.ElementContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#repeat}.
	 * @param ctx the parse tree
	 */
	void enterRepeat(@NotNull AbnfParser.RepeatContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#repeat}.
	 * @param ctx the parse tree
	 */
	void exitRepeat(@NotNull AbnfParser.RepeatContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#rule_}.
	 * @param ctx the parse tree
	 */
	void enterRule_(@NotNull AbnfParser.Rule_Context ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#rule_}.
	 * @param ctx the parse tree
	 */
	void exitRule_(@NotNull AbnfParser.Rule_Context ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#rulelist}.
	 * @param ctx the parse tree
	 */
	void enterRulelist(@NotNull AbnfParser.RulelistContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#rulelist}.
	 * @param ctx the parse tree
	 */
	void exitRulelist(@NotNull AbnfParser.RulelistContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#group}.
	 * @param ctx the parse tree
	 */
	void enterGroup(@NotNull AbnfParser.GroupContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#group}.
	 * @param ctx the parse tree
	 */
	void exitGroup(@NotNull AbnfParser.GroupContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#alternation}.
	 * @param ctx the parse tree
	 */
	void enterAlternation(@NotNull AbnfParser.AlternationContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#alternation}.
	 * @param ctx the parse tree
	 */
	void exitAlternation(@NotNull AbnfParser.AlternationContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#concatenation}.
	 * @param ctx the parse tree
	 */
	void enterConcatenation(@NotNull AbnfParser.ConcatenationContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#concatenation}.
	 * @param ctx the parse tree
	 */
	void exitConcatenation(@NotNull AbnfParser.ConcatenationContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#elements}.
	 * @param ctx the parse tree
	 */
	void enterElements(@NotNull AbnfParser.ElementsContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#elements}.
	 * @param ctx the parse tree
	 */
	void exitElements(@NotNull AbnfParser.ElementsContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#option}.
	 * @param ctx the parse tree
	 */
	void enterOption(@NotNull AbnfParser.OptionContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#option}.
	 * @param ctx the parse tree
	 */
	void exitOption(@NotNull AbnfParser.OptionContext ctx);

	/**
	 * Enter a parse tree produced by {@link AbnfParser#repetition}.
	 * @param ctx the parse tree
	 */
	void enterRepetition(@NotNull AbnfParser.RepetitionContext ctx);
	/**
	 * Exit a parse tree produced by {@link AbnfParser#repetition}.
	 * @param ctx the parse tree
	 */
	void exitRepetition(@NotNull AbnfParser.RepetitionContext ctx);
}