# Summary

Objective-C 2.0 grammar with preprocessor support.

# Parser Stages

* Preprocessor Lexing (ObjectiveCPreprocessorLexer.g4).
* Preprocessor Parsing (ObjectiveCPreprocessorLexer.g4).
* Removing directives from code (ObjectiveCPreprocessor.java).
* Lexing (ObjectiveCLexer.g4).
* Parsing (ObjectiveCParser.g4).

`ObjectiveCPreprocessor` calculates preprocessor directive values and replaces not compilied code with spaces.
These spaces are using for proper error line and column detection during the ordinary Objective-C code parsing.

# Testing

More than 95% correctly parsed ***.m** files from these projects:

1. AFNetworking: [2a53b2c3f49275352677dfd46a35b0287ae01458](https://github.com/AFNetworking/AFNetworking/tree/2a53b2c3f49275352677dfd46a35b0287ae01458)
2. realm-cocoa: [1b3da1a23a17973c5c3030b293e9d9eea2e8a306](https://github.com/realm/realm-cocoa/tree/1b3da1a23a17973c5c3030b293e9d9eea2e8a306)
3. iOS-Source-Code-Analyze: [89816b258b5b80824694f4041e04c4402dc1d6ac](https://github.com/Draveness/iOS-Source-Code-Analyze/tree/89816b258b5b80824694f4041e04c4402dc1d6ac)
4. SDWebImage: [c012fc9bb2e5b1775a8a4a847938352187496a05](https://github.com/rs/SDWebImage/tree/c012fc9bb2e5b1775a8a4a847938352187496a05)
5. MJRefresh: [62cb56c97432e5c5bc1f0b5de0f2b2f1c5c929dd](https://github.com/CoderMJLee/MJRefresh/tree/62cb56c97432e5c5bc1f0b5de0f2b2f1c5c929dd)
6. SmarterStreaming [87c6ceb31bf0fb6b4c31187017c6007dfcdad83c](https://github.com/daniulive/SmarterStreaming/tree/87c6ceb31bf0fb6b4c31187017c6007dfcdad83c)
7. ReactiveCocoa: [c3487f9427f0c7bbf332b4f8501eb5185119cb00](https://github.com/ReactiveCocoa/ReactiveCocoa/tree/c3487f9427f0c7bbf332b4f8501eb5185119cb00)
8. GSKStretchyHeaderView: [d24efa77b327ef7c25b6975fe705da99e12131da](https://github.com/gskbyte/GSKStretchyHeaderView/tree/d24efa77b327ef7c25b6975fe705da99e12131da)
9. JSQMessagesViewController: [f3ae3b290071ac0be7221c1b6550b2db93288b9d](https://github.com/jessesquires/JSQMessagesViewController/tree/f3ae3b290071ac0be7221c1b6550b2db93288b9d)
10. SocketRocket: [5ce5f5d1d8c828e66f0d892146a291fbf3d34199](https://github.com/facebook/SocketRocket/tree/5ce5f5d1d8c828e66f0d892146a291fbf3d34199)
11. MBProgressHUD: [3de6e1398f0ab7e5a4375549f946870c844cdba2](https://github.com/jdg/MBProgressHUD/tree/3de6e1398f0ab7e5a4375549f946870c844cdba2)
12. DZNEmptyDataSet: [441a84146f8c615eabede3801c920d774f9289d6](https://github.com/dzenbot/DZNEmptyDataSet/tree/441a84146f8c615eabede3801c920d774f9289d6)
13. AsyncDisplayKit: [f72f39b4453883632ba33d2d3fd922ddb21d9dd9](https://github.com/facebook/AsyncDisplayKit/tree/f72f39b4453883632ba33d2d3fd922ddb21d9dd9)
14. UITableView-FDTemplateLayoutCell: [e3ee86ce419d18d3ff735056f1474f2863e43003](https://github.com/forkingdog/UITableView-FDTemplateLayoutCell/tree/e3ee86ce419d18d3ff735056f1474f2863e43003)
15. SDAutoLayout: [df61d42c9be5fa6b5edd9086de2b8d64e668d003](https://github.com/gsdios/SDAutoLayout/tree/df61d42c9be5fa6b5edd9086de2b8d64e668d003)
16. Chameleon: [3f87b407bb84ad5e23a0ee8a2ef4023ebb62b00a](https://github.com/ViccAlexander/Chameleon/tree/3f87b407bb84ad5e23a0ee8a2ef4023ebb62b00a)
17. TomatoRead: [4389af9767ac5169e151f7fec4bdfc015e098b9f](https://github.com/everettjf/TomatoRead/tree/4389af9767ac5169e151f7fec4bdfc015e098b9f)
18. iCarousel: [7739680a08028d29250c540b91b896dac23a92cd](https://github.com/nicklockwood/iCarousel/tree/7739680a08028d29250c540b91b896dac23a92cd)
19. SVProgressHUD: [6ca5946638ed5bbf8ae36e3c3191901f76bf173b](https://github.com/SVProgressHUD/SVProgressHUD/tree/6ca5946638ed5bbf8ae36e3c3191901f76bf173b)
20. fmdb: [345b1936c553be01f749eebb14bf4b42c5e4158f](https://github.com/ccgus/fmdb/tree/345b1936c553be01f749eebb14bf4b42c5e4158f)

# License

ObjectiveC grammars are licensed under [MIT](https://opensource.org/licenses/MIT).