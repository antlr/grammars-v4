use antlr4_runtime::{CharStream, LexerSemCtx, SemanticHooks};

pub struct LexerBase;

impl LexerBase {
    pub fn new() -> Self {
        Self
    }
}

impl SemanticHooks for LexerBase {
    const ENABLES_LEXER_LIFECYCLE: bool = true;

    fn lexer_sempred<I>(
        &mut self,
        ctx: &mut LexerSemCtx<'_, I>,
        rule_index: usize,
        pred_index: usize,
    ) -> Option<bool>
    where
        I: CharStream,
    {
        match (rule_index, pred_index) {
            // NCNameChar (rule 31), predicate 0: { this.Check1() }?
            // Same lookahead test as the CSharp LexerBase.
            (31, 0) => Some(ctx.la(1) != ':' as i32),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::LexerBase;
    use antlr4_runtime::{CommonTokenStream, InputStream, Token};
    use crate::r#gen::rex_lexer::{RexLexer, METADATA};

    #[test]
    fn predicate_coordinates_match_the_grammar() {
        assert_eq!(METADATA.rule_names()[31], "NCNameChar");
    }

    #[test]
    fn check1_preserves_csharp_lookahead_behavior() {
        for (input, expected) in [("AB ", "AB"), ("AB", "AB"), ("AB::=", "A")] {
            let lexer = RexLexer::with_hooks(InputStream::new(input), LexerBase::new());
            let tokens = CommonTokenStream::new(lexer);
            let first = tokens.tokens().next().unwrap();
            assert_eq!(first.text_or_empty(), expected, "input: {input}");
        }
    }

    #[test]
    fn processing_instruction_preserves_questions_and_restores_mode() {
        let lexer = RexLexer::with_hooks(
            InputStream::new("<?target body???>Next"), LexerBase::new());
        let tokens = CommonTokenStream::new(lexer);
        let text: Vec<_> = tokens.tokens()
            .filter(|t| t.token_type() != crate::r#gen::rex_lexer::EOF)
            .map(|t| t.text_or_empty().to_owned())
            .collect();
        assert_eq!(text, ["<?", "target", " ", "body", "?", "?", "?>", "Next"]);
    }
}
