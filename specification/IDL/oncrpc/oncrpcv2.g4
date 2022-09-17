grammar oncrpcv2;
import xdr;

//oncrpcv2 additions on top of xdr (rfc 5531)
programDef: 'program' IDENTIFIER '{'
        versionDef
        versionDef*
    '}' '=' constant ';';
versionDef: 'version' IDENTIFIER '{'
        procedureDef
        procedureDef*
    '}' '=' constant ';';
procedureDef: procReturn IDENTIFIER '(' procFirstArg (',' typeSpecifier)* ')' '=' constant ';';
procReturn: 'void' | typeSpecifier;
procFirstArg: 'void' | typeSpecifier;
oncrpcv2Specification : (xdrSpecification | programDef)* EOF; //this is the top level rule for oncrpcv2 (rfc 5531)
