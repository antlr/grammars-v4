#import "AppDelegate.h"
@import AFNetworking;

// directives

#if !__has_feature(objc_arc)
#error SVProgressHUD is ARC only. Either turn on ARC for the project or use -fobjc-arc flag
#endif // Comment in directive

#define __IPHONE_OS_VERSION_MIN_REQUIRED 70000  /* Comment in directive */

#if __IPHONE_OS_VERSION_MIN_REQUIRED >= 60000
    #define MBLabelAlignmentCenter NSTextAlignmentCenter
#else
    #define MBLabelAlignmentCenter UITextAlignmentCenter
#endif

#ifndef NSFoundationVersionNumber_iOS_8_0
    #define NSFoundationVersionNumber_With_Fixed_5871104061079552_bug 1140.11
#else
    #define NSFoundationVersionNumber_With_Fixed_5871104061079552_bug NSFoundationVersionNumber_iOS_8_0
#endif


#if __has_feature(objc_arc)
	#define MB_AUTORELEASE(exp) exp
	#define MB_RELEASE(exp) exp
	#define MB_RETAIN(exp) exp
#else
	#define MB_AUTORELEASE(exp) [exp autorelease]
	#define MB_RELEASE(exp) [exp release]
	#define MB_RETAIN(exp) [exp retain]
#endif

// Source: https://github.com/AFNetworking/AFNetworking/blob/e1e2d62ad80602706cae48ed4d391faa0d5edbdf/AFNetworking/AFHTTPSessionManager.h#L74
NS_ASSUME_NONNULL_BEGIN
@interface AFHTTPSessionManager : AFURLSessionManager <NSSecureCoding, NSCopying>
@end
NS_ASSUME_NONNULL_END

// Source: https://github.com/AFNetworking/AFNetworking/blob/32cf30a2451917c25d853fccb9e112b64415595e/AFNetworking/AFNetworkReachabilityManager.h#L27
typedef NS_ENUM(NSInteger, AFNetworkReachabilityStatus) {
    AFNetworkReachabilityStatusUnknown          = -1,
    AFNetworkReachabilityStatusNotReachable     = 0,
    AFNetworkReachabilityStatusReachableViaWWAN = 1 << 0,
    AFNetworkReachabilityStatusReachableViaWiFi = 2,
};

// Source:https://github.com/wymsee/cordova-HTTP/blob/master/src/ios/AFNetworking/AFURLSessionManager.m#L31
static dispatch_queue_t url_session_manager_creation_queue() {
    dispatch_once(&onceToken, ^{
        af_url_session_manager_creation_queue = dispatch_queue_create("com.alamofire.networking.session.manager.creation", DISPATCH_QUEUE_SERIAL);
    });

    bodyPart.bodyContentLength = (unsigned long long)length;
}

// Source: https://github.com/AFNetworking/AFNetworking/blob/b7073e4990b93ccdd1f16a5ebbb5443a193b002d/Tests/Tests/AFSecurityPolicyTests.m#L146
static SecTrustRef AFUTTrustWithCertificate(SecCertificateRef certificate) {
    NSArray *certs  = @[(__bridge id)(certificate)];
    SecPolicyRef policy = SecPolicyCreateBasicX509();
    SecTrustRef trust = NULL;
    SecTrustCreateWithCertificates((__bridge CFTypeRef)(certs), policy, &trust);
    CFRelease(policy);
    return trust;
}

// Source: https://github.com/AFNetworking/AFNetworking/blob/32cf30a2451917c25d853fccb9e112b64415595e/AFNetworking/AFURLResponseSerialization.h#L269
@interface AFCompoundResponseSerializer : AFHTTPResponseSerializer
+ (instancetype)compoundSerializerWithResponseSerializers:(NSArray <id<AFURLResponseSerialization>> *)responseSerializers;
@end

// Source: https://github.com/AFNetworking/AFNetworking/blob/32cf30a2451917c25d853fccb9e112b64415595e/UIKit%2BAFNetworking/AFNetworkActivityIndicatorManager.h#L44
NS_EXTENSION_UNAVAILABLE_IOS("Use view controller based solutions where appropriate instead.")
@interface AFNetworkActivityIndicatorManager : NSObject
@end

// realm-cocoa

// Source: https://github.com/realm/realm-cocoa/blob/master/examples/ios/objc/Backlink/AppDelegate.m#L28
@interface Dog : RLMObject
@property NSString *name;
@property NSInteger age;
@property (readonly) RLMLinkingObjects *owners;
@end
RLM_ARRAY_TYPE(Dog)

// Source: https://github.com/realm/realm-cocoa/blob/master/examples/ios/objc/Encryption/LabelViewController.m#L100
@implementation LabelViewController
// Log a message to the screen since we can't just use NSLog() with no debugger attached
- (void)log:(NSString *)format, ... {
    va_list args;
    va_start(args, format);
    NSString *str = [[NSString alloc] initWithFormat:format arguments:args];
    va_end(args);
    self.textView.text = [[self.textView.text
                           stringByAppendingString:str]
                           stringByAppendingString:@"\n\n"];
}
@end

// Source: https://github.com/realm/realm-cocoa/blob/master/Realm/RLMAccessor.h#L27
// #ifdef __cplusplus
// typedef NSUInteger RLMCreationOptions;
// #else
// typedef NS_OPTIONS(NSUInteger, RLMCreationOptions);
// #endif

// SDWebImage

// Source: https://github.com/rs/SDWebImage/blob/master/SDWebImage/SDWebImageDownloaderOperation.h#L37
@interface SDWebImageDownloaderOperation : NSOperation <SDWebImageOperation, NSURLSessionTaskDelegate, NSURLSessionDataDelegate>
@property (nonatomic, assign) BOOL shouldUseCredentialStorage __deprecated_msg("Property deprecated. Does nothing. Kept only for backwards compatibility");
@end

// Source: https://github.com/rs/SDWebImage/blob/master/SDWebImage/MKAnnotationView%2BWebCache.h#L112
@interface MKAnnotationView (WebCacheDeprecated)
- (NSURL *)imageURL __deprecated_msg("Use `sd_imageURL`");
@end

// Source: https://github.com/rs/SDWebImage/blob/master/SDWebImage/SDWebImageCompat.h
#if __IPHONE_OS_VERSION_MIN_REQUIRED != 20000 && __IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_5_0
#error SDWebImage doesn't support Deployment Target version < 5.0
#endif

#if OS_OBJECT_USE_OBJC
    #undef SDDispatchQueueRelease
    #undef SDDispatchQueueSetterSementics
    #define SDDispatchQueueRelease(q)
    #define SDDispatchQueueSetterSementics strong
#else
#undef SDDispatchQueueRelease
#undef SDDispatchQueueSetterSementics
#define SDDispatchQueueRelease(q) (dispatch_release(q))
#define SDDispatchQueueSetterSementics assign
#endif

#define dispatch_main_sync_safe(block)\
    if ([NSThread isMainThread]) {\
        block(\"-Warc-performSelector-leaks\");\
    } else {\
        dispatch_sync(dispatch_get_main_queue(), block);\
    }
    
#import <GSKStretchyHeaderView/GSKStretchyHeaderView.h>

// Source: https://github.com/rs/SDWebImage/blob/master/SDWebImage/SDWebImageManager.h#L288
typedef void(^SDWebImageCompletedBlock)(UIImage *image, NSError *error, SDImageCacheType cacheType) __deprecated_msg("Block type deprecated. Use `SDWebImageCompletionBlock`");

// MJRefresh
// Source: https://github.com/CoderMJLee/MJRefresh/blob/master/MJRefresh/MJRefreshConst.h#L11
// 日志输出
#ifdef DEBUG
#define MJRefreshLog(...) NSLog(__VA_ARGS__)
#else
#define MJRefreshLog(...)
#endif

// Source: https://github.com/CoderMJLee/MJRefresh/blob/master/MJRefreshExample/Classes/First/MJSingleViewController.m#L24
@implementation MJSingleViewController
- (void)viewDidLoad {
    [super viewDidLoad];
    __unsafe_unretained typeof(self) weakSelf = self;
    __unsafe_unretained __typeof(self) weakSelf = self;
}
@end


// GSKStretchyHeaderView
// Source: https://github.com/gskbyte/GSKStretchyHeaderView/blob/master/Example/Pods/Expecta/Expecta/ExpectaSupport.m#L70
id _EXPObjectify(const char *type, ...) {
    if(strcmp(type, @encode(__typeof__(nil))) == 0) {
        obj = nil;
    }
}

// SocketRocket

typedef enum SRStatusCode : NSInteger {
    SRStatusCodeNormal = 1000,
} SRStatusCode;

@implementation MJSingleViewController
- (void)viewDidLoad {
    [super viewDidLoad];
    __unsafe_unretained typeof(self) weakSelf = self;
    __unsafe_unretained __typeof(self) weakSelf = self;
}
@end

@implementation SRDelegateController
- (void)performDelegateBlock:(SRDelegateBlock)block
{
    // Source:https://github.com/facebook/SocketRocket/blob/master/SocketRocket/Internal/Delegate/SRDelegateController.m
    __block __strong id<SRWebSocketDelegate> delegate = nil;
    __block SRDelegateAvailableMethods availableMethods = {};
    
    // Source: https://github.com/facebook/SocketRocket/blob/master/Tests/SRAutobahnTests.m
    IMP implementation = imp_implementationWithBlock(^(SRAutobahnTests *self) {
        [self performTestWithCaseNumber:caseNumber identifier:identifier];
    });
}
@end

// MBProgressHUD
// Source:  https://github.com/jdg/MBProgressHUD/blob/master/MBProgressHUD.m#L784
// https://github.com/dzenbot/DZNEmptyDataSet/blob/master/Examples/WebBrowser/Pods/DZNWebViewController/Source/Classes/DZNWebViewController.m#L460
@implementation MBBarProgressView
- (id)init {
    return [self initWithFrame:CGRectMake(.0f, .0f, 120.0f, 20.0f, 0ul)];
}

- (void)testGraceTime {
    // https://github.com/jdg/MBProgressHUD/blob/master/Demo/HudTests/HudTests.m
    XCTAssertEqualObjects(hud.superview, rootView, @"The hud should be added to the view."); \
}
@end

// DZNEmptyDataSet
// Source: 

// AsyncDisplayKit
// Source: https://github.com/facebook/AsyncDisplayKit/blob/master/AsyncDisplayKit/ASDisplayNodeExtras.h#L137
extern NSArray<__kindof ASDisplayNode *> *ASDisplayNodeFindAllSubnodesOfClass(ASDisplayNode *start, Class c);

// Source: https://github.com/facebook/AsyncDisplayKit/blob/master/AsyncDisplayKit/ASRunLoopQueue.h#L17
@interface ASRunLoopQueue<ObjectType> : NSObject
@end

// AFNetworking
// Source: https://github.com/gsdios/SDAutoLayout/blob/master/SDAutoLayoutDemo/DemoVC/DemoVC10/Lib/AFNetworking/AFNetworking/AFNetworking.h#L39
#if ( ( defined(__MAC_OS_X_VERSION_MAX_ALLOWED) && __MAC_OS_X_VERSION_MAX_ALLOWED >= 1090) || \
      ( defined(__IPHONE_OS_VERSION_MAX_ALLOWED) && __IPHONE_OS_VERSION_MAX_ALLOWED >= 70000 ) || \
       TARGET_OS_WATCH )
    #import "AFURLSessionManager.h"
#endif

// Source: https://github.com/gsdios/SDAutoLayout/blob/master/SDAutoLayoutDemo/DemoVC/DemoVC11/ChatController/Models/SDChatModel.h#L30
typedef enum : NSUInteger {
    SDMessageTypeSendToOthers,
    SDMessageTypeSendToMe
} SDMessageType;

REGULAREXPRESSION(URLRegularExpression,@"((http[s]{0,1}|ftp)://[a-zA-Z0-9\\.\\-]+\\.([a-zA-Z]{2,4})(:\\d+)?(/[a-zA-Z0-9\\.\\-~!@#$%^&*+?:_/=<>]*)?)|(www.[a-zA-Z0-9\\.\\-]+\\.([a-zA-Z]{2,4})(:\\d+)?(/[a-zA-Z0-9\\.\\-~!@#$%^&*+?:_/=<>]*)?)")


// Source: https://github.com/SVProgressHUD/SVProgressHUD/blob/master/SVProgressHUD/SVProgressHUD.m#L839
#pragma mark - Master show/dismiss methods

// Source: https://github.com/ReactiveCocoa/ReactiveCocoa/blob/7877f99bdfb4be1c82c4804082e99c35d0a93a91/ReactiveCocoa/Objective-C/RACEagerSequence.m#L17
@implementation RACEagerSequence
+ (instancetype)return:(id)value {
	return [[self sequenceWithArray:@[ value ] offset:0] setNameWithFormat:@"+return: %@", RACDescription(value)];
}
@end

// Source: https://github.com/facebook/AsyncDisplayKit/blob/05dba2263cf86f7fce42f1918aa513385232f34c/AsyncDisplayKit/Details/ASWeakSet.m#L15
@interface ASWeakSet<__covariant ObjectType> ()
@property (nonatomic, strong, readonly) NSMapTable<ObjectType, NSNull *> *mapTable;
@end
