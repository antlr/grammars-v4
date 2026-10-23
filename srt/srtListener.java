// Generated from srt.g4 by ANTLR 4.13.2
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link srtParser}.
 */
public interface srtListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link srtParser#file_}.
	 * @param ctx the parse tree
	 */
	void enterFile_(srtParser.File_Context ctx);
	/**
	 * Exit a parse tree produced by {@link srtParser#file_}.
	 * @param ctx the parse tree
	 */
	void exitFile_(srtParser.File_Context ctx);
	/**
	 * Enter a parse tree produced by {@link srtParser#subtitles}.
	 * @param ctx the parse tree
	 */
	void enterSubtitles(srtParser.SubtitlesContext ctx);
	/**
	 * Exit a parse tree produced by {@link srtParser#subtitles}.
	 * @param ctx the parse tree
	 */
	void exitSubtitles(srtParser.SubtitlesContext ctx);
	/**
	 * Enter a parse tree produced by {@link srtParser#subtitle}.
	 * @param ctx the parse tree
	 */
	void enterSubtitle(srtParser.SubtitleContext ctx);
	/**
	 * Exit a parse tree produced by {@link srtParser#subtitle}.
	 * @param ctx the parse tree
	 */
	void exitSubtitle(srtParser.SubtitleContext ctx);
}