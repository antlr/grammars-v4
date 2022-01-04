# Summary

Objective-C 2.0 grammars with preprocessor support can be used in 2 different modes:

* One-step processing. Includes a lexer which is used for 
both directives and ordinary Objective-C code.
Used in [Swiftify](https://objectivec2swift.com/) Objective-C to Swift converter.
* Two-step processing. Includes separate lexers for directives and Objective-C code.
Used in [Codebeat](https://codebeat.co/), the web service for automated code review
for different programming languages (including Objective-C).

## One-step processing

1. Lexer (ObjectiveCLexer.g4).
2. Parser (ObjectiveCPreprocessorParser.g4 and ObjectiveCParser.g4).

## Two-step processing

1. Preprocessor lexer (ObjectiveCPreprocessorLexer.g4).
2. Preprocessor parser (ObjectiveCPreprocessorParser.g4).
3. Visitor class for removing preprocessor directives from code (ObjectiveCPreprocessor.java).
4. Lexer (ObjectiveCLexer.g4).
5. Parser (ObjectiveCParser.g4).

`ObjectiveCPreprocessor` processes preprocessor directives and replaces code
that does not compile with spaces. 
Spaces are kept for correct detection of error line and column numbers
during parsing of the ordinary Objective-C code. 
`ObjectiveCParser` is shared by both one-step and two-step processing.

# Testing

## One-step processing

Successful parsing and conversion of complete projects,
ex. [Popovers demo](https://objectivec2swift.com/#/home/faq#how-do-i-convert-an-entire-project-from-objective-c-to-swift).

## Two-step processing

More than 95% correctly parsed **.m** files from these projects:

1. [AFNetworking](https://github.com/AFNetworking/AFNetworking)
2. [realm-cocoa](https://github.com/realm/realm-cocoa)
3. [iOS-Source-Code-Analyze](https://github.com/Draveness/iOS-Source-Code-Analyze)
4. [SDWebImage](https://github.com/rs/SDWebImage)
5. [MJRefresh](https://github.com/CoderMJLee/MJRefresh)
6. [SmarterStreaming](https://github.com/daniulive/SmarterStreaming)
7. [ReactiveCocoa](https://github.com/ReactiveCocoa/ReactiveCocoa)
8. [GSKStretchyHeaderView](https://github.com/gskbyte/GSKStretchyHeaderView)
9. [JSQMessagesViewController](https://github.com/jessesquires/JSQMessagesViewController)
10. [SocketRocket](https://github.com/facebook/SocketRocket)
11. [MBProgressHUD](https://github.com/jdg/MBProgressHUD)
12. [DZNEmptyDataSet](https://github.com/dzenbot/DZNEmptyDataSet)
13. [AsyncDisplayKit](https://github.com/facebook/AsyncDisplayKit)
14. [UITableView-FDTemplateLayoutCell3](https://github.com/forkingdog/UITableView-FDTemplateLayoutCell)
15. [SDAutoLayout](https://github.com/gsdios/SDAutoLayout)
16. [Chameleon](https://github.com/ViccAlexander/Chameleon)
17. [TomatoRead](https://github.com/everettjf/TomatoRead)
18. [iCarousel](https://github.com/nicklockwood/iCarousel)
19. [SVProgressHUD](https://github.com/SVProgressHUD/SVProgressHUD)
20. [fmdb](https://github.com/ccgus/fmdb)

# License

ObjectiveC grammars and helper files are licensed under [MIT](https://opensource.org/licenses/MIT).