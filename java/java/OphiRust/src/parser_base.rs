use antlr4_runtime::{ParserSemCtx, TokenSource};

use crate::r#gen::java_parser::{self as parser, JavaParserHooks, RecordComponentListContext};

#[derive(Clone, Copy, Debug, Default)]
pub struct ParserBase;

impl ParserBase {
    pub const fn new() -> Self {
        Self
    }
}

impl JavaParserHooks for ParserBase {
    fn is_not_identifier_assign<L>(&mut self, ctx: &mut ParserSemCtx<'_, L>) -> bool
    where
        L: TokenSource,
    {
        let identifier_like = matches!(
            ctx.la(1),
            parser::IDENTIFIER
                | parser::MODULE
                | parser::OPEN
                | parser::REQUIRES
                | parser::EXPORTS
                | parser::OPENS
                | parser::TO
                | parser::USES
                | parser::PROVIDES
                | parser::WHEN
                | parser::WITH
                | parser::TRANSITIVE
                | parser::YIELD
                | parser::SEALED
                | parser::PERMITS
                | parser::RECORD
                | parser::VAR
        );
        !identifier_like || ctx.la(2) != parser::ASSIGN
    }

    fn do_last_record_component<L>(&mut self, ctx: &mut ParserSemCtx<'_, L>) -> bool
    where
        L: TokenSource,
    {
        let Some(context) = ctx.context() else {
            return true;
        };
        let Some(list) = antlr4_runtime::generated::__active_context_view::<
            RecordComponentListContext<'_, antlr4_runtime::generated::__ActiveParserContext>,
        >(
            context,
            Vec::new(),
            ctx.parse_tree_storage(),
            ctx.token_store(),
        ) else {
            return true;
        };

        let components: Vec<_> = list.record_component_children().collect();
        let count = components.len();
        components
            .iter()
            .enumerate()
            .all(|(index, component)| component.ellipsis_token().is_none() || index + 1 == count)
    }
}
