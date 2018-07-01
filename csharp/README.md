## Summary

C# grammar with full support of
[C# 6 features](https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6) and below.
CSharpLexer.g4 and CSharpPreprocessor.g4 are runtime dependent (contains specific actions),
because of directive preprocessors and string interpolation (these things make grammar context-sensitive).
C# and Java versions supported. But CSharpParser.g4 is runtime independent.
This ANTLR 4 C# grammar is based on the C# ANTLR 3 grammar from
[ChristianWulf/CSharpGrammar](https://github.com/ChristianWulf/CSharpGrammar).

## Using

Due to uncompiled code can be placed after such preprocessor directives:
```CSharp
#if SILVERLIGHT && WINDOWS_PHONE || DEBUG || foo == true
// Some code
#endif
```
at first stage it's necessary to calculate whether condition satisfied in directive.
This step performed by runtime code in gist:
[CSharpAntlrParser](https://gist.github.com/KvanTTT/d95579de257531a3cc15).

## Testing

I put into repository **AllInOne.cs** file (from Roslyn) with most common syntax and also
I tested this grammar on the following most popular .NET projects:
* [Roslyn-1.1.1](https://github.com/dotnet/roslyn).
* [Corefx-1.0.0-rc2](https://github.com/dotnet/corefx).
* [EntityFramework-7.0.0-rc1](https://github.com/aspnet/EntityFramework).
* [aspnet-mvc-6.0.0-rc1](https://github.com/aspnet/Mvc).
* [ImageProcessor-2.3.0](https://github.com/JimBobSquarePants/ImageProcessor).
* [Newtonsoft.Json-8.0.2](https://github.com/JamesNK/Newtonsoft.Json).

There are some problems with deep recursion ([TestData.g.cs in CoreFx](https://github.com/dotnet/corefx/blob/master/src/Common/tests/System/Xml/XmlCoreTest/TestData.g.cs)),
unicode chars
([sjis.cs in  Roslyn](https://github.com/dotnet/roslyn/blob/master/src/Compilers/Test/Resources/Core/Encoding/sjis.cs))
and preprocessor directives.

ANTLR parser from this grammar approximately ~5-7 slowly than Roslyn parser.

## License

[Eclipse Public License - v 1.0](http://www.eclipse.org/legal/epl-v10.html)
