module namespace lsp = "lsp";

declare record lsp:Position(
	line as number,
	character as number
);

declare type lsp:PositionEncodingKind as enum('utf-8', 'utf-16', 'utf-32');

declare record lsp:Range(
	"start" as lsp:Position,
	"end" as lsp:Position
);

declare function lsp:start-position($node as node()?) as lsp:Position? {
    $node ! lsp:Position(
        line := antlr:line() - 1,
        character := antlr:pos()
    )
};

declare function lsp:end-position($node as node()?) as lsp:Position? {
    $node ! (
        (:TODO: remove treatment after grained call return type analysis :)
        let $start-line := antlr:line(.) - 1
        let $start-pos := antlr:pos(.)
        let $string-node := $node=>string()
        let $new-lines := $string-node=>characters()=>index-of("\n")
        let $additional-lines := $new-lines=>count()
        let $last-line-index := $new-lines[$additional-lines]
        let $last-line-length := string-length(
                if ($last-line-index=>exists())
                    then substring($string-node, $new-lines[$additional-lines] treat as number)
                    else $string-node
            )
        return lsp:Position(
            line := $start-line + $additional-lines,
            character := $start-pos + $last-line-length
        )
    )
};

declare function lsp:range($node as node()?) as lsp:Range? {
    if ($node) then
        lsp:Range(
            start := lsp:start-position($node),
            end := lsp:end-position($node)
        )
    else ()
};
