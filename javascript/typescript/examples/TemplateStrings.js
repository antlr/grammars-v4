var empty = ``;

var simple = `foobar`;

var lineBreak = `
`;

var nested = `aaa${`bbb`}ccc`;

// https://stackoverflow.com/questions/67618203/antlr4-parsing-templateliteral-in-jsx/67632704?noredirect=1#comment119548351_67632704
let str =
    `${dsName}${parameterStr ? `( ${parameterStr} )` : ""}${returns ? `{
${returns}}` : ""}`;

// https://github.com/antlr/grammars-v4/issues/2978
let templateStringWithEscapes = `\\ \` \n \$`;

// https://github.com/antlr/grammars-v4/issues/2978
let templateStringWithEscapedLineBreak = `\
`;

// https://github.com/antlr/grammars-v4/issues/3051
let nestedBracesTest = `${[1, 2, 3].map(x => { return x * 2;}).join("")}`;