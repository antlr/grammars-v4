// https://github.com/antlr/grammars-v4/issues/3772
for (var i = 0; i < [].length; i++) {
    var o = {}
    if (false) continue
    o[test]
}