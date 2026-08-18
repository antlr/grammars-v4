// Default no-op lexer base. Grammars that use superClass replace this file
// with a grammar-specific lexer_base.rs via transformGrammar.py.
use antlr4_runtime::SemanticHooks;

pub struct LexerBase;

impl LexerBase {
    pub fn new() -> Self {
        Self
    }
}

impl SemanticHooks for LexerBase {}
