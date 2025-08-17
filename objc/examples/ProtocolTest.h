#import <Foundation/Foundation.h>

// Test typedef block
typedef void (^ABCharactorCompletionBlock)(NSMutableDictionary *characterData, NSString *versionedId);
typedef unsigned char HelloWorld;
// Test nullability specifier in typedef
typedef void (^CompletionBlock)(NSString *_Nonnull param);

// Test consts
static CGSize const kButtonSize = {50, 50};

NS_ASSUME_NONNULL_BEGIN

// Test protocol alias in Swift
NS_SWIFT_NAME(FooMeow)
@protocol ABFoo <NSObject, ABAnotherProtocol>

// Test inline declaration of block
@property (nonatomic, copy, nullable) void (^handler_resultsForQuery)
    (Query *, void (^)(ABResult<QueryResultModel *> *));
        
@property (nonatomic, assign, nonnull) BOOL (^enumerateProviders)(id<ABProviderProtocol> provider, BOOL *stop);

@property (nonatomic, copy, nullable, readonly) NSString *string;

@property (nonatomic, copy, nonnull) id<ABProviderProtocol> provider;
        
// Test blocks
- (void)performBlock:(CompletionBlock)block;

// Test NS_NOESCAPE
- (void)noEscapeBlock:(NS_NOESCAPE CompletionBlock)block;

- (void)dispatchWithBlock:(dispatch_block_t)block;

// Test api availability and swift alias attributes
- (void)startFoo:(AdFoo *)foo
        completionHandler:(nullable void (^)(NSError *_Nullable error))completion
    NS_SWIFT_NAME(startFooMeow(_:completionHandler:))API_AVAILABLE(ios(14.5))
        API_UNAVAILABLE(macos, watchos)__TVOS_PROHIBITED;

-(instancetype)init __attribute__((unavailable("init is not available.")));
+(instancetype) new __attribute__((unavailable("new is not available")));
-(void)f __attribute__((availability(macosx,introduced=10.4,deprecated=10.6,message="hello world")));


@optional

// Test compound id types
- (void)compoundType:(id<Foo, Bar> _Nonnull)fooBar;

// Nullability specifiers
- (nullable NSNumber *)someTypeWithOptionalStr:(nullable NSString *)optionalStr optionalStr2:(NSString *_Nullable)str2;

@end

NS_ASSUME_NONNULL_END