<if(has_name_space)>namespace <name_space>
{<endif>
    public class ErrorListener<Symbol> : IAntlrErrorListener<Symbol>
    {
        /// <summary>
        /// Provides a default instance of
        /// <see cref="ErrorListener{Symbol}"/>
        /// .
        /// </summary>
        public static readonly ErrorListener<Symbol> Instance = new ErrorListener<Symbol>();

        /// <summary>
        /// <inheritDoc/>
        /// <p>
        /// This implementation prints messages to
        /// <see cref="System.Console.Out"/>
        /// containing the
        /// values of
        /// <paramref name="line"/>
        /// ,
        /// <paramref name="charPositionInLine"/>
        /// , and
        /// <paramref name="msg"/>
        /// using
        /// the following format.</p>
        /// <pre>
        /// line <em>line</em>:<em>charPositionInLine</em> <em>msg</em>
        /// </pre>
        /// </summary>
        public virtual void SyntaxError(IRecognizer recognizer, Symbol offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            System.Console.Out.WriteLine("line " + line + ":" + charPositionInLine + " " + msg);
        }
    }
<if(has_name_space)>}<endif>