use antlr4_runtime::{CharStream, LexerLifecycleCtx, LexerSemCtx, SemanticHooks, Token, TokenView};

use crate::r#gen::awk_lexer::{
    BUILTIN_FUNC_NAME, CHANNEL_DEFAULT_TOKEN_CHANNEL, DECR, INCR, NUMBER, RB, RP, STRING, WORD,
};

pub struct AwkLexerBase {
    after_expr: bool,
}

impl AwkLexerBase {
    pub fn new() -> Self {
        Self { after_expr: false }
    }
}

impl SemanticHooks for AwkLexerBase {
    const ENABLES_LEXER_LIFECYCLE: bool = true;

    fn lexer_sempred<I>(
        &mut self,
        _ctx: &mut LexerSemCtx<'_, I>,
        rule_index: usize,
        pred_index: usize,
    ) -> Option<bool>
    where
        I: CharStream,
    {
        match (rule_index, pred_index) {
            // ERE rule (rule index 59, predicate 0): {this.IsNotAfterExpr()}?
            (59, 0) => Some(!self.after_expr),
            _ => None,
        }
    }

    fn lexer_token_emitted(&mut self, token: TokenView<'_>) {
        if token.channel() == CHANNEL_DEFAULT_TOKEN_CHANNEL {
            let t = token.token_type();
            self.after_expr = t == WORD
                || t == NUMBER
                || t == STRING
                || t == BUILTIN_FUNC_NAME
                || t == INCR
                || t == DECR
                || t == RP
                || t == RB;
        }
    }

    fn lexer_reset<I>(&mut self, _ctx: &mut LexerLifecycleCtx<'_, I>)
    where
        I: CharStream,
    {
        self.after_expr = false;
    }
}
