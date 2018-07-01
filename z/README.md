## Summary

Z Notation grammar based on the [ISO standard](http://standards.iso.org/ittf/PubliclyAvailableStandards/c021573_ISO_IEC_13568_2002%28E%29.zip) with [corrections](https://www.iso.org/obp/ui/#iso%3Astd%3Aiso%2Diec%3A13568%3Aed%2D1%3Av1%3Acor%3A1%3Av1%3Aen)

## Examples

I created standard\_toolkit\_operator\_templates.utf8 based upon "Annex B" in the ISO standard. All other examples were created from [CZT's test cases](https://git.code.sf.net/p/czt/code)

## Using

The ISO version of Z Notation is context sensitive with regard to operators.  After lexing the input stream, you need to run ZOperatorParser.g4 to convert the appropriate name tokens into operator tokens.  Finally you can run ZParser.g4 on the modified token stream.  Test.java has an example of this process.

## Notes

In Test.java I always prepend standard\_toolkit\_operator\_templates.utf8 to the input.  The specification says these aren't always supposed to be mapped, but should only be mapped if your Z section has no parents, or only parts of it should be mapped if a subsection is listed as a parent instead of the entire toolkit.  This functionality is not implemented.  

Precedence values specified in Z operator templates are not taken into consideration.  I'm not sure this is possible with ANTLR4.

Associativity values specified in Z operator templates are taken into consideration, and do seem to work correctly, but I'm not sure how they are working.  They are specified like:

```ANTLR
| expression {ZSupport.isLeftAssociative(_input)}? I expression #InfixLeftApplicationExpression
| <assoc=right> expression I expression #InfixRightApplicationExpression
```

that is, the semantic predicate is not on the left side of the rule, so my understanding from this [SO post](http://stackoverflow.com/a/23677069/7711235) is that the predicate will be ignored and not used in prediction...maybe it's consuming the first expression and falling through to the next rule?


