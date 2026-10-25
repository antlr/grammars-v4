// https://github.com/antlr/grammars-v4/issues/3772
for (var i = 0; i < [].length; i++) {
    var o = {}
    if (false) continue
    o[test]
    
    if (false) continue// comment
    o[test]
    
    if (false) continue // comment
    o[test]
    
    if (false) continue // comment
    
    o[test]
    
    if (false) continue/*
         				* multiline comment
         				*/
    o[test]
    
    if (false) continue /*
         				 * multiline comment
         				*/
    o[test]
}

mylabel: for (var i = 0; i < [].length; i++) {	
    if (true) continue mylabel;
}