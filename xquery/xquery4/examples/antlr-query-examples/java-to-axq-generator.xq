declare variable $type-maps as map(string, string) := {
    'String': 'string',
    'boolean': 'boolean',
    'char': 'string',
    'int': 'number',
    'byte': 'number',
    'short': 'number',
    'int': 'number',
    'long': 'number',
    'float': 'number',
    'double': 'number'
};

(
    for $record in //recordDeclaration
    let $recordName := $record/typeIdentifier => string()
    let $fields := $record//recordComponent
    let $fieldnames := $fields//identifier => string(),
        $types := $fields//unannType => string()

    return
        "declare record " || $recordName || "(&#10;    " || (
            for $fieldname at $i in $fieldnames
            let $type := $types[$i]
            return "$" || $fieldnames[$i] || " as " || ($type-maps?$type otherwise $type)
          ) => string-join(",&#10;    ") ||
        "&#10;);"
) => string-join("&#10;")
