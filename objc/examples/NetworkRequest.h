//
//  NetworkRequest.h
//  FlickStackr
//
//  Created by Carlos Mejía on 11-09-01.
//  Copyright 2011 iPont. All rights reserved.
//

#import <Foundation/Foundation.h>

@class NetworkRequest;
@class OAuthToken;

extern NSString *NR_BODY_PARAM;
extern NSString *NR_CONTENT_TYPE_PARAM;
extern NSString *NR_HTML;
extern NSString *NR_MULTIPART_PARAM;

// Block Style
typedef void (^NetworkRequestDictionaryResponse)(NSDictionary* response, NSError *error);
typedef void (^NetworkRequestStringResponse)(NSString* response, NSError *error);
typedef void (^NetworkRequestDataResponse)(NSData* data, NSError *error);
typedef void (^NetworkRequestFileResponse)(NSString* filename, NSError *error);
typedef void (^NetworkRequestProgress)(NSUInteger bytes, NSUInteger totalBytes);
typedef void (^NetworkUploadSetupCompleted)(void);
typedef NSError* (^NetworkRequestFailedParse)(NSString* stringResponse, NSInteger tries);

typedef NS_ENUM(NSInteger, NetworkRequestResponseType)
{
    ResponseJSON=0, ResponseXML=1, ResponseString=2, ResponseData=3, ResponseFile=4
} ;

typedef NS_ENUM(NSInteger, NetworkRequestRequestType)
{
    RequestGet=1, RequestPost=2, RequestPostForm=6, RequestPut=8, RequestDelete=16, RequestJSON=32, RequestPostJSON=34, RequestPutJSON=40, RequestPostBody=66, RequestMove=128, RequestMoveJSON=160
} ;

typedef enum _NetworkRequestError
{
    OFFlickrAPIRequestConnectionError = 0x7fff0001,
	OFFlickrAPIRequestFaultyXMLResponseError = 0x7fff0003,
    ErrorJSONNotJSON=0x7fff0003, /* <- SameAsflickrError*/
    ErrorJSON1=0x7fff0004,
    ErrorXmlNotXml = 0x7fff0005,
    ErrorHttp400 = 0x7fff0006,
    ErrorHttp200 = 0x7fff0007,
    ErrorEmptyMessage = 0x7fff0028
} NetworkRequestError;

@interface NetworkRequest : NSObject<NSURLConnectionDelegate>
{
}
-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType;
-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters;
-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType  postParameters:(NSDictionary*)inParameters queryParameters:(NSDictionary*)queryParams headers:(NSDictionary*)headers ;
-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters consumer:(OAuthToken*)consumer token:(OAuthToken*)token;
-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters consumer:(OAuthToken*)consumer token:(OAuthToken*)token specialOAuthEncoding:(BOOL)inSpecialOAuthEncoding ;
-(instancetype)initForUpload:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType imageStream:(NSInputStream *)inImageStream  parameters:(NSDictionary *)inParameters fileSize:(long long)fileSize consumer:(OAuthToken*)consumer token:(OAuthToken*)token ;
-(instancetype)initForUpload:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType  imageStream:(NSInputStream *)inImageStream suggestedFilename:(NSString *)inFilename filePropertyName:(NSString*)filePropertyName MIMEType:(NSString *)inType parameters:(NSDictionary *)inArguments queryParameters:(NSDictionary *)inQueryArguments consumer:(OAuthToken*)consumer token:(OAuthToken*)token includeParamsInSignature:(BOOL)includeParamsInSignature ;
-(void)cancel;
-(void)openWithDictionaryResponse:(NetworkRequestDictionaryResponse)inHandler;
-(void)openWithDictionaryResponse:(NetworkRequestDictionaryResponse)inHandler progress:(NetworkRequestProgress)progress failedParse:(NetworkRequestFailedParse)failedParse backgroundNotification:(BOOL)backgroundNotification;
-(void)openWithStringResponse:(NetworkRequestStringResponse)inHandler;
-(void)openWithDataResponse:(NetworkRequestDataResponse)inHandler progress:(NetworkRequestProgress)progress;
-(void)openWithFileResponse:(NSString*)filename handler:(NetworkRequestFileResponse)inHandler progress:(NetworkRequestProgress)progress;
-(void)removeNetworkIndicator;
- (void)backgroundRequestCompleted:(NSData*)responseData fileName:(NSURL*)localFile;
-(void)handleError:(NSError*)error httpReturnCode:(NSInteger)httpStatus  headers:(NSDictionary*)headers;
@property (nonatomic, readonly, copy) NSURL *postBackgroundFilename;
+(NSNumber*)nextRequestId;
+(void)networkRequestStarted;
+(void)networkRequestStopped;
@property(nonatomic) NSNumber *key;
@property(nonatomic) BOOL hasFailed;
@property(nonatomic,assign) BOOL specialOAuthEncoding;
@property(nonatomic,assign) BOOL backgroundRequest;
@property(nonatomic,assign) BOOL ignoreRedirect;
@property(nonatomic,readonly) NSURL *url;
@property(nonatomic,strong) NSString *keyUrlString;
@property(nonatomic,readonly) NSString *urlString;
@property(nonatomic,readonly) NSURLRequest *urlRequest;
@property(nonatomic,copy) NetworkRequestProgress progressHandler;
@property(nonatomic,readonly) NetworkRequestRequestType requestType;
@end
