//
//  NetworkRequest.m
//  FlickStackr
//
//  Created by Carlos Mejía on 11-09-01.
//  Copyright 2011 iPont. All rights reserved.
//

#import "NetworkRequest.h"
#import "FSXMLMapper.h"
#import "IPEnv.h"
#import "BackgroundNetworkManager.h"

// NEEDED TEMPORARILY TO WORKAROUND ISSUES WITH 500px

/*
@implementation NSURLRequest (IgnoreSSL)

+ (BOOL)allowsAnyHTTPSCertificateForHost:(NSString *)host
{
    return YES;
}

@end
*/

// NEEDED TEMPORARILY TO WORKAROUND ISSUES WITH 500px

NSString *NR_BODY_PARAM = @"body";
NSString *NR_CONTENT_TYPE_PARAM = @"contentType";
NSString *NR_MULTIPART_PARAM = @"multipartType";

extern BOOL IPONT_DEBUG_REQUESTS;

@interface NetworkRequest()
{
    NSURLConnection *urlConnection;
    NSMutableData *urlResponseData;
    NSString *desiredDownloadFilename;
    NSOutputStream* downloadFileStream;
    NSInteger tries;
    NSInteger fileLength;
    NSInteger expectedLength;
    NetworkRequest *selfReference;      // while the response is being delivered, to make sure this object does not die in the middle
    BOOL networkIndicatorStarted;
    BOOL backgroundNotificationForDictionary;
    BOOL hasSentProgressForUpload;
    BOOL uploadRequest;
    BOOL backgroundProcessing;
    NSDictionary<NSString*,id> *parametersForOAuth;
    OAuthToken *oAuthConsumer;
    OAuthToken *oAuthToken;
    NSMutableURLRequest *urlRequest;
    NetworkRequestResponseType responseType;
    NSString *uploadTempFilename;
    double startTime;
}
@property(nonatomic,copy) NetworkRequestDictionaryResponse dictionaryHandler;
@property(nonatomic,copy) NetworkRequestStringResponse stringHandler;
@property(nonatomic,copy) NetworkRequestDataResponse dataHandler;
@property(nonatomic,copy) NetworkRequestFileResponse fileHandler;
@property(nonatomic,copy) NetworkRequestFailedParse failedParseHandler;
@end


@implementation NetworkRequest
@synthesize key;
@synthesize urlRequest;
@synthesize requestType;
// static globals
static int32_t              requestCount = 0;
static int32_t              signatureCount = 0;
static int32_t              networkActivityCount = 0;
static NSDictionary* requestTypeToHTTPString;
static NSObject *networkCountMonitor;

// constants
static const NSInteger      retryLimit = 1;
static const NSTimeInterval defaultNetworkRequestTimeout = 90;
static const NSTimeInterval defaultNetworkRequestUploadTimeout = 1200;

+(void)initialize
{
    if (!requestTypeToHTTPString)
        requestTypeToHTTPString = @{@(RequestGet):@"GET", @(RequestPost):@"POST", @(RequestPostForm):@"POST", @(RequestPut):@"PUT", @(RequestDelete):@"DELETE", @(RequestPostJSON):@"POST", @(RequestPutJSON):@"PUT",  @(RequestPostBody):@"POST", @(RequestMove):@"MOVE", @(RequestMoveJSON):@"MOVE"};
    if (!networkCountMonitor)
        networkCountMonitor = [[NSObject alloc] init];
    if (signatureCount==0)
        signatureCount = (int32_t) ([[NSDate date] timeIntervalSince1970]);
}

+(NSNumber*)nextRequestId
{
    NSInteger reqNumber = OSAtomicIncrement32(&requestCount);
    return @(reqNumber);
}

+(NSString*)requestTypeToHTTPString:(NetworkRequestRequestType)reqType
{
    return requestTypeToHTTPString[@(reqType)];
}


+(void)networkRequestStarted
{
    @synchronized(networkCountMonitor)
    {
        if (networkActivityCount == 0)
            [UIApplication sharedApplication].networkActivityIndicatorVisible = YES;
        networkActivityCount++;
    }
}
+(void)networkRequestStopped
{
    @synchronized(networkCountMonitor)
    {
        networkActivityCount--;
        if (networkActivityCount < 0)
            networkActivityCount = 0;

        if (networkActivityCount == 0)
            [UIApplication sharedApplication].networkActivityIndicatorVisible = NO;
    }
}


- (instancetype)initForUpload:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType imageStream:(NSInputStream *)inImageStream suggestedFilename:(NSString *)inFilename filePropertyName:(NSString*)filePropertyName MIMEType:(NSString *)inType parameters:(NSDictionary *)inArguments queryParameters:(NSDictionary *)inQueryArguments consumer:(OAuthToken*)consumer token:(OAuthToken*)token includeParamsInSignature:(BOOL)includeParamsInSignature
{
    if (self = [super init])
    {
        uploadRequest = YES;
        responseType = inResponseType;
        requestType = inRequestType;
        self.key = [NetworkRequest nextRequestId];
        oAuthToken = (token) ? token : [[OAuthToken alloc] init];
        oAuthConsumer = consumer;

        if (inQueryArguments)
            url = [self buildFullUrl:url queryParameters:inQueryArguments];
        
        [self prepareUploadImageStream:inImageStream suggestedFilename:inFilename filePropertyName:filePropertyName MIMEType:inType parameters:inArguments url:url includeParamsInSignature:includeParamsInSignature];
    }
    return self;
}

- (instancetype)initForUpload:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType imageStream:(NSInputStream *)inImageStream  parameters:(NSDictionary *)inParameters fileSize:(long long)fileSize consumer:(OAuthToken*)consumer token:(OAuthToken*)token 
{
    if (self = [self initWithUrl:url responseType:inResponseType requestType:RequestPost parameters:inParameters consumer:consumer token:token])
    {
        uploadRequest = YES;
        [urlRequest setValue:@"application/octet-stream" forHTTPHeaderField:@"Content-Type"];
        [urlRequest addValue:[@(fileSize) stringValue] forHTTPHeaderField: @"Content-Length"];
        
        if ([IPEnv isOs7])
        {
            // write the stream to disk, so ios7 can upload it in the background
            NSOutputStream *outputStream = [self createTempFile];
            [self writeInputStream:inImageStream toOutputStream:outputStream];
            [outputStream close];
        }
        else
            [urlRequest setHTTPBodyStream:inImageStream];
        urlRequest.timeoutInterval = defaultNetworkRequestUploadTimeout;
    }
    return self;
}
- (instancetype)initWithUrl:(NSString*)url  responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType postParameters:(NSDictionary*)inParameters queryParameters:(NSDictionary*)queryParams headers:(NSDictionary*)headers
{
    if (self = [super init])
    {
        responseType = inResponseType;
        requestType = inRequestType;
        self.key = [NetworkRequest nextRequestId];

        // for get requests append the parameters to the URL
        if (queryParams.count )
            url = [self buildFullUrl:url queryParameters:queryParams];
        
        [self createRequestWithUrl:url];       // this sets urlRequest
        
        if (inParameters)
        {
            if (inRequestType == RequestPostForm)
                [self addFormData:inParameters];
            else if ((inRequestType & RequestJSON)!=0)
                [self addFormJSONData:inParameters];
            else if (inRequestType == RequestPostBody)
                [self addFormFullBodyData:inParameters];
        }

        // add requested cookies
        if (headers)
            [urlRequest setAllHTTPHeaderFields:headers];
    }
    return self;
}

-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters consumer:(OAuthToken*)consumer token:(OAuthToken*)token
{
    return (self = [self initWithUrl:url responseType:inResponseType requestType:inRequestType parameters:parameters consumer:consumer token:token specialOAuthEncoding:NO]);
}
-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters consumer:(OAuthToken*)consumer token:(OAuthToken*)token specialOAuthEncoding:(BOOL)inSpecialOAuthEncoding
{
    if (self = [super init])
    {
        responseType = inResponseType;
        requestType = inRequestType;
        oAuthToken = (token) ? token : [[OAuthToken alloc] init];
        oAuthConsumer = consumer;

        self.key = [NetworkRequest nextRequestId];
        if (requestType != RequestPostForm && parameters.count)
            url = [self buildFullUrl:url queryParameters:parameters];
        
        [self createRequestWithUrl:url];       // this sets urlRequest

        if (requestType == RequestPostForm && parameters.count)
            [self addFormData:parameters];

        parametersForOAuth = parameters;
        self.specialOAuthEncoding = inSpecialOAuthEncoding;
        [self prepareOAuth];
    }
    return self;
}

- (instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType 
{
    return [self initWithUrl:url responseType:inResponseType requestType:RequestGet postParameters:nil queryParameters:nil headers:nil];
}

- (instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters headers:(NSDictionary*)headers 
{
    return [self initWithUrl:url responseType:inResponseType requestType:inRequestType postParameters:parameters queryParameters:nil headers:headers];
}

-(instancetype)initWithUrl:(NSString*)url responseType:(NetworkRequestResponseType)inResponseType requestType:(NetworkRequestRequestType)inRequestType parameters:(NSDictionary*)parameters
{
    return [self initWithUrl:url responseType:inResponseType requestType:inRequestType postParameters:parameters queryParameters:nil headers:nil];
}

-(NSString*)buildFullUrl:(NSString*)baseUrl queryParameters:(NSDictionary*)parameters
{
    return [NSString stringWithFormat:@"%@?%@",baseUrl,[self buildParamsString:parameters]];
}

-(void)createRequestWithUrl:(NSString*)url
{
    NSMutableURLRequest *request = [[NSMutableURLRequest alloc] initWithURL:[NSURL URLWithString:url] cachePolicy:NSURLRequestReloadIgnoringLocalCacheData timeoutInterval:defaultNetworkRequestTimeout];
    request.HTTPMethod = requestTypeToHTTPString[@(requestType)];
    urlRequest = request;
}

-(void)openWithDictionaryResponse:(NetworkRequestDictionaryResponse)inHandler
{
    [self openWithDictionaryResponse:inHandler progress:nil failedParse:nil backgroundNotification:NO];
}

-(void)openWithDictionaryResponse:(NetworkRequestDictionaryResponse)inHandler progress:(NetworkRequestProgress)progress failedParse:(NetworkRequestFailedParse)failedParse backgroundNotification:(BOOL)backgroundNotification
{
    self.dictionaryHandler = inHandler;
    self.progressHandler = progress;
    self.failedParseHandler = failedParse;
    backgroundNotificationForDictionary = backgroundNotification;
    [self startRequest];
}

-(void)openWithStringResponse:(NetworkRequestStringResponse)inHandler
{
    self.stringHandler = inHandler;
    [self startRequest];
}
 
-(void)openWithDataResponse:(NetworkRequestDataResponse)inHandler progress:(NetworkRequestProgress)progress
{
    self.dataHandler = inHandler;
    self.progressHandler = progress;
    [self startRequest];
}

-(void)openWithFileResponse:(NSString*)fileName handler:(NetworkRequestFileResponse)inHandler progress:(NetworkRequestProgress)progress
{
    responseType = ResponseFile;
    desiredDownloadFilename = fileName;
    self.fileHandler = inHandler;
    self.progressHandler = progress;
    [self startRequest];
}


-(void)startRequest
{
    if (IPONT_DEBUG_REQUESTS)
    {
        startTime = CACurrentMediaTime();
        NSLog(@"REQUEST %@: %@", key, self.urlString);
    }
    
    networkIndicatorStarted = YES;
    [NetworkRequest networkRequestStarted];
    
    BackgroundNetworkManager *backManager = [BackgroundNetworkManager instance];
    if ([IPEnv isOs7] && (uploadRequest || self.backgroundRequest || backManager.applicationInBackground))
    {
        backgroundProcessing = YES;
        [backManager addRequest:self];
        return;
    }
    
    urlConnection = [[NSURLConnection alloc] initWithRequest:urlRequest delegate:self startImmediately:NO];
    
    // make sure the request is started on the main thread
    runOnMainThread(^
    {
        [urlConnection scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSRunLoopCommonModes];
        [urlConnection start];
    });
}

-(void)removeNetworkIndicator
{
    networkIndicatorStarted = NO;
    [NetworkRequest networkRequestStopped];
}

-(NSOutputStream*)createTempFile
{
    if (desiredDownloadFilename)
        uploadTempFilename = [NSTemporaryDirectory() stringByAppendingString:desiredDownloadFilename] ;
    else
        uploadTempFilename = [NSTemporaryDirectory() stringByAppendingFormat:@"%@.%@", @"ca.ipont.upload", [self generateUniqueId]] ;
    
    // create the write stream
    NSOutputStream *outputStream = [NSOutputStream outputStreamToFileAtPath:uploadTempFilename append:NO];
    [outputStream open];
    return outputStream;
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSHTTPURLResponse *)response
{
    if (responseType==ResponseData || responseType==ResponseFile)
    {
        if ((response.statusCode>=400 && ![response.allHeaderFields[@"Content-Type"] hasPrefix:@"image"]) || response.statusCode > 200) // google and box may return this
        {
            // check if the content type is image then continue, Picasa does that
            [connection cancel];
            [self handleError:nil httpReturnCode:response.statusCode headers:response.allHeaderFields];
            return;
        }
    }
    
    NSNumber *length = [response allHeaderFields][@"Content-Length"];
    if (responseType == ResponseFile)
    {
        expectedLength = [length integerValue];
        downloadFileStream = [self createTempFile];
    }
    else if (length)
    {
        expectedLength = [length integerValue];
        urlResponseData = [[NSMutableData alloc] initWithCapacity:expectedLength];
    }
    else
    {
        urlResponseData = [[NSMutableData alloc] init];
    }
}
- (NSURLRequest *)connection:(NSURLConnection *)inConnection willSendRequest: (NSURLRequest *)inRequest redirectResponse: (NSURLResponse *)inRedirectResponse
{
    if (inRedirectResponse && self.ignoreRedirect && self.stringHandler)
    {
        NSData *urlData = [[inRequest.URL absoluteString] dataUsingEncoding:NSUTF8StringEncoding];
        [self processStringReponse:urlData];
        // the error handler will be removed, so although an error is created, it will be lost
        return nil;
    }
    return inRequest;
}

-(void)writeNSDataToFileStream:(NSData*)data
{
    fileLength += data.length;
    NSInteger offset = 0;
    NSInteger bytesToWrite = data.length;
    NSInteger bytesWritten = 0;
    do
    {
        offset += bytesWritten;
        bytesToWrite -= bytesWritten;
        bytesWritten = [downloadFileStream write:(data.bytes+offset) maxLength:bytesToWrite];
    }
    while (bytesWritten != bytesToWrite);
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    NSUInteger sentLength=0;
    if (downloadFileStream)
    {
        [self writeNSDataToFileStream:data];
        sentLength = fileLength;
    }
    else
    {
        [urlResponseData appendData:data];
        sentLength = urlResponseData.length;
    }
    if (expectedLength && self.progressHandler && !hasSentProgressForUpload)
    {
        self.progressHandler(sentLength,expectedLength);
    }
}

- (void)connection:(NSURLConnection *)connection didSendBodyData:(NSInteger)bytesWritten totalBytesWritten:(NSInteger)totalBytesWritten totalBytesExpectedToWrite:(NSInteger)totalBytesExpectedToWrite
{
    if (self.progressHandler)
    {
        hasSentProgressForUpload = YES;  // make sure we don't update on upload and on the response
        self.progressHandler(totalBytesWritten, totalBytesExpectedToWrite);
    }
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection willCacheResponse:(NSCachedURLResponse *)cachedResponse
{
    // we don't want to cache anything. we'll cache what's needed in higher level format
    return nil;
}

-(void)releaseHandlers
{
    self.dataHandler = nil;
    self.stringHandler = nil;
    self.fileHandler = nil;
    self.dictionaryHandler = nil;
    self.progressHandler = nil;
    self.failedParseHandler = nil;
    selfReference = nil;

}
-(void)releaseConnectionAndData
{
    if (networkIndicatorStarted)
    {
        [NetworkRequest networkRequestStopped];
        networkIndicatorStarted = NO;
    }
	urlConnection = nil;
	urlResponseData = nil;
    urlRequest = nil;
    [self cleanUpTempFile];
}

-(void)handleError:(NSError*)error httpReturnCode:(NSInteger)httpStatus headers:(NSDictionary*)headers
{
    if (IPONT_DEBUG_REQUESTS)
    {
        NSLog(@"REQUEST %@: ERROR %f %@", key, CACurrentMediaTime() - startTime, error);
    }

    IP_RETAIN(self);
	{
        self.hasFailed = YES;
        
        if (httpStatus || (error && error.code <= NSURLErrorBadURL && error.code >= NSURLErrorBadServerResponse))
        {
            if (httpStatus==0 && tries < retryLimit)
            {
                BETA(NSLog(@"REQUEST %@: WILLRETRY %@", key, urlRequest.URL.absoluteString));
                [self sendRetry];
            }
            else
            {
                // Turn it into our communication error for easier handling
                BETA(NSLog(@"REQUEST %@: ERROR : %@ # %@",key, urlRequest.URL.absoluteString, error));
                NSError *newError;
                if (httpStatus)
                {
                    NSString *errorMsg = [NSString stringWithFormat:@"%@:HTTP/%d",urlRequest.URL.host, (int)httpStatus];
                    if (!headers)
                        headers = @{};
                    
                    newError = [NSError errorWithDomain:@"ipont.ca" code:((httpStatus>=400) ? ErrorHttp400 : ErrorHttp200) userInfo:@{NSLocalizedFailureReasonErrorKey: errorMsg, NSLocalizedDescriptionKey: errorMsg, @"headers":headers}];
                }
                else
                {
                    newError = [NSError errorWithDomain:@"ipont.ca" code:OFFlickrAPIRequestConnectionError userInfo:error.userInfo];
                }
                [self sendFailure:newError];
                [self releaseConnectionAndData];
            }
        }
        else
        {
            [self sendFailure:error];
            [self releaseConnectionAndData];
        }
	}
    IP_RELEASE();
}
- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    [self handleError:error httpReturnCode:0 headers:nil];
}

-(void)sendSuccessDictionary:(NSDictionary*)response
{
    IPSimpleBlock sendSuccess = ^(void)
    {
        if (self.dictionaryHandler)
            self.dictionaryHandler(response,nil);
        [self releaseHandlers];
    };
    
    if (backgroundNotificationForDictionary)
        sendSuccess();
    else
        runAsyncOnMainThread(sendSuccess);
}

-(void)sendFailure:(NSError*)error
{
    runOnMainThread(^
    {
        BETA(NSLog(@"REQUEST %@: ERROR %@", key, urlRequest.URL.absoluteString));

        if (self.dictionaryHandler)
            self.dictionaryHandler(nil,error);
        else if (self.dataHandler)
            self.dataHandler(nil,error);
        else if (self.stringHandler)
            self.stringHandler(nil,error);
        else if (self.fileHandler)
            self.fileHandler(nil,error);
        
        [self releaseHandlers];
    });
}

-(void)sendRetry
{
    // do it on main thread
    runAsyncOnMainThread(^
    {
        tries++;
        if (oAuthConsumer)
            [self prepareOAuth];  //re-sign it

        BETA(NSLog(@"REQUEST %@: RETRY %@", key, urlRequest.URL.absoluteString));

        [self startRequest];
        [NetworkRequest networkRequestStopped]; // to balance it
    });
}
-(BOOL)handleFailedParse:(NSString*)responseString
{
    NSError *errorToReturn;
    
    if (self.failedParseHandler)
    {
        errorToReturn = self.failedParseHandler(responseString, tries);
        if (!errorToReturn && tries < retryLimit)
        {
            BETA(NSLog(@"REQUEST %@: FAILED PARSE %@", key, responseString));
            [self sendRetry];
            return YES;
        }
    }
    self.failedParseHandler = nil;
    
    // no error built, then do one here
    if (!errorToReturn)
    {
        NSInteger code = responseString.length==0 ? ErrorEmptyMessage : ErrorJSONNotJSON;
        NSString *message = (!responseString) ? @"" : responseString;
        if (!message.length)
            message = [NSString stringWithFormat:IPLS(@"GeneralNetworkError"),self.url.host];
        errorToReturn = [NSError errorWithDomain:@"ipont.ca" code:code userInfo:@{NSLocalizedFailureReasonErrorKey: message, NSLocalizedDescriptionKey: message}];
        self.hasFailed = YES;
        
        BETA(NSLog(@"REQUEST %@ JSONError : %@ # %@ # %@",key, urlRequest.URL.absoluteString, errorToReturn, responseString));
    }
    [self sendFailure:errorToReturn];
    return NO;
}

-(void)processJSONResponse:(NSData *)data
{
    runOnWorkerThread(^
    {
       @autoreleasepool
       {
           NSDictionary* jsonDict = (data.length) ? [NSJSONSerialization JSONObjectWithData:data options:0 error:nil] : [[NSDictionary alloc] init];
           if (!jsonDict)
           {
               NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
               if (![self handleFailedParse:responseString])
                   [self releaseConnectionAndData];
           }
           else
           {
               [self releaseConnectionAndData];
               [self sendSuccessDictionary:jsonDict];
           }
       }
    });
}

-(void)processXMLResponse:(NSData *)data
{
    runOnWorkerThread(^
    {
       @autoreleasepool
       {
           NSDictionary* xmlDict = nil;
           if (data.length)
           {
               xmlDict = [FSXMLMapper dictionaryMappedFromXMLData:data];
               if (!xmlDict)
               {
                   // first try turning it into ascii
                   NSString *xmlString = [[NSString alloc] initWithData:data encoding:NSASCIIStringEncoding];
                   NSData *newData = [xmlString dataUsingEncoding:NSUTF8StringEncoding allowLossyConversion:YES];
                   xmlDict = [FSXMLMapper dictionaryMappedFromXMLData:newData];
               }
           }
           
           if (!xmlDict)
           {
               NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
               if (requestType == RequestDelete && responseString.length==0)
               {
                   xmlDict = [[NSDictionary alloc] init];
               }
               else
               {
                   if (![self handleFailedParse:responseString])
                   {
                       // we have already sent the failed response
                       [self releaseConnectionAndData];
                   }
                   return;
               }
           }
           // send success
           [self releaseConnectionAndData];
           [self sendSuccessDictionary:xmlDict];
       }
    });
}

-(void)processStringReponse:(NSData *)data
{
    NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    @autoreleasepool
    {
        if (self.stringHandler)
            self.stringHandler(responseString, nil);
        [self releaseConnectionAndData];
        [self releaseHandlers];
    }
}

- (void)processDataResponse:(NSData*)data
{
    runOnWorkerThread(^
    {
        @autoreleasepool
        {
            if (self.dataHandler)
                self.dataHandler(data, nil);
            [self releaseConnectionAndData];
            [self releaseHandlers];
        }
    });
}


- (void)processFileResponse:(NSOutputStream*)fileStream
{
    [fileStream close];  // close the file
    if (self.fileHandler)
    {
        @autoreleasepool
        {
            self.fileHandler(uploadTempFilename, nil);
        }
    }
    [self releaseHandlers]; // don't cleanup just connection/temp info because the delegate may use the file later (releaseConnectionAndData)
 
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    [self requestCompleted:urlResponseData];
}

- (void)backgroundRequestCompleted:(NSData*)responseData fileName:(NSURL*)localFile
{
    if (!responseData)
    {
        if (responseType == ResponseFile)
        {
            NSOutputStream *stream = [self createTempFile];
            [stream close];
            NSFileManager *fileMgr = [NSFileManager defaultManager];
            [fileMgr removeItemAtPath:uploadTempFilename error:nil];
            [fileMgr copyItemAtPath:localFile.path toPath:uploadTempFilename error:nil];
            [self requestCompleted:nil];
            return;
        }
        responseData = [NSData dataWithContentsOfURL:localFile];
    }
    
    [self requestCompleted:responseData];
}

- (void)requestCompleted:(NSData*)responseData
{
    if (IPONT_DEBUG_REQUESTS)
    {
        NSLog(@"REQUEST %@: TIME %0.3f LEN %ld", key, CACurrentMediaTime() - startTime, (long)responseData.length);
    }
    
    if (self.dictionaryHandler || self.dataHandler || self.fileHandler || self.stringHandler)
    {
        selfReference = self;
        switch (responseType)
        {
            case ResponseJSON:
                [self processJSONResponse:responseData];
                break;
            case ResponseXML:
                [self processXMLResponse:responseData];
                break;
            case ResponseString:
                [self processStringReponse:responseData];
                break;
            case ResponseData:
                [self processDataResponse:responseData];
                break;
            case ResponseFile:
                [self processFileResponse:downloadFileStream];
                break;
        }
    }
}

- (void)cleanUpTempFile
{
    if (downloadFileStream)
    {
        [downloadFileStream close];
        downloadFileStream = nil;
    }
    
    if (uploadTempFilename)
    {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        if ([fileManager fileExistsAtPath:uploadTempFilename]) 
        {
			NSError *error = nil;
			[fileManager removeItemAtPath:uploadTempFilename error:&error];
        }
        uploadTempFilename = nil;
    }
}

-(void)cancel
{
    if (backgroundProcessing)
        [[BackgroundNetworkManager instance] cancelRequest:self];
    
    [urlConnection cancel];
    
    [self releaseConnectionAndData];
    [self releaseHandlers];
}

-(NSMutableString*)buildParamsString:(NSDictionary*)params
{
    NSMutableString * buildString = [[NSMutableString alloc] initWithCapacity:params.count*30];
    for (NSString* eachName in params)
    {
        id valueRaw = params[eachName];
        
        if ([valueRaw isKindOfClass:[NSArray class]])
        {
            for (id eachValue in valueRaw)
            {
                if (buildString.length>0)
                    [buildString appendString:@"&"];

                [buildString appendFormat:@"%@=%@",eachName,[eachValue stringValue].URLEncodedString];
            }
        }
        else
        {
            NSString *value = [valueRaw stringValue];
            if (value)
            {
                if (buildString.length>0)
                    [buildString appendString:@"&"];
                [buildString appendFormat:@"%@=%@", eachName.URLEncodedString,value.URLEncodedString];
            }
        }
    }
    return buildString;
}

-(void)addFormJSONData:(NSDictionary *)inArguments
{
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:inArguments options:0 error:nil];
    [urlRequest setHTTPBody: jsonData];
    [urlRequest setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
}

-(void)addFormFullBodyData:(NSDictionary *)inArguments
{
    if (inArguments.count!=2)
        return;

    NSString *bodyValue = inArguments[NR_BODY_PARAM];
    const char *jsonUTF8 = [bodyValue UTF8String];
    size_t writeLength = strlen(jsonUTF8);
    
    NSData *myRequestData = [NSData dataWithBytes:jsonUTF8 length:writeLength];
    [urlRequest setHTTPBody: myRequestData];
    [urlRequest setValue:inArguments[NR_CONTENT_TYPE_PARAM] forHTTPHeaderField:@"Content-Type"];
}

-(void)addFormData:(NSDictionary *)inArguments
{
    NSString *formBody = [self buildParamsString:inArguments];
    const char *formUTF8 = [formBody UTF8String];
    size_t formUTF8Length = strlen(formUTF8);

    NSData *myRequestData = [NSData dataWithBytes:formUTF8 length:formUTF8Length];
    [urlRequest setHTTPBody: myRequestData];
    [urlRequest setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
}

-(void)writeInputStream:(NSInputStream*)inImageStream toOutputStream:(NSOutputStream*)outputStream
{
    // open the input stream
    const size_t bufferSize = 65536;
    NSInteger readSize = 0;
    uint8_t *buffer = (uint8_t *)calloc(1, bufferSize);
    
    [inImageStream open];
    while ([inImageStream hasBytesAvailable])
    {
        if (!(readSize = [inImageStream read:buffer maxLength:bufferSize]))
        {
            break;
        }
		[outputStream write:buffer maxLength:readSize];
    }
    
    [inImageStream close];
    free(buffer);
}

- (BOOL)prepareUploadImageStream:(NSInputStream *)inImageStream suggestedFilename:(NSString *)inFilename filePropertyName:(NSString*)filePropertyName MIMEType:(NSString *)mimeType parameters:(NSDictionary *)inArguments url:(NSString*)urlString includeParamsInSignature:(BOOL)includeParamsInSignature
{
    NSString *separator = [self generateUniqueId];
    NSString *multiPartType = @"form-data";
    // build the multipart form
    NSMutableString *multipartBegin = [NSMutableString string];
    NSMutableString *multipartEnd = [NSMutableString string];
    
    if ((inArguments.count == 2 || inArguments.count==3) && inArguments[NR_CONTENT_TYPE_PARAM] && inArguments[NR_BODY_PARAM])
    {
        // only two parameters, which are the content type and body itself. Used by google technology
        [multipartBegin appendFormat:@"--%@\r\nContent-Type: %@\r\n\r\n%@\r\n", separator, inArguments[NR_CONTENT_TYPE_PARAM],inArguments[NR_BODY_PARAM]];
        NSString *multiPart = inArguments[NR_MULTIPART_PARAM];
        if (multiPart)
            multiPartType = multiPart;
    }
    else
    {
        for (NSString* eachName in inArguments)
        {
            NSString *value = inArguments[eachName];
            if (value)
                [multipartBegin appendFormat:@"--%@\r\nContent-Disposition: form-data; name=\"%@\"\r\n\r\n%@\r\n", separator, eachName, value];
        }
    }
    NSString *contentType = [NSString stringWithFormat:@"multipart/%@; boundary=%@", multiPartType, separator];
    
    // add filename, if nil, generate a UUID
    [multipartBegin appendFormat:@"--%@\r\nContent-Disposition: form-data; name=\"%@\"; filename=\"%@\"\r\n", separator, filePropertyName, [inFilename length] ? inFilename : [self generateUniqueId]];
    [multipartBegin appendFormat:@"Content-Type: %@\r\n\r\n", mimeType];
    [multipartEnd appendFormat:@"\r\n--%@--\r\n", separator];
    
    // now we have everything, create a temp file for this purpose; although UUID is inferior to
    
	[self cleanUpTempFile];
    
    // create the write stream
    NSOutputStream *outputStream = [self createTempFile];

    // write the header
    const char *UTF8String = [multipartBegin UTF8String];
    size_t writeLength = strlen(UTF8String);
	[outputStream write:(uint8_t *)UTF8String maxLength:writeLength];
	
    // write the file tiself
    [self writeInputStream:inImageStream toOutputStream:outputStream];
    
    
    UTF8String = [multipartEnd UTF8String];
    writeLength = strlen(UTF8String);
	[outputStream write:(uint8_t *)UTF8String maxLength:writeLength];
    [outputStream close];
    
    NSError *error = nil;
    NSDictionary *fileInfo = [[NSFileManager defaultManager] attributesOfItemAtPath:uploadTempFilename error:&error];
    NSNumber *fileSizeNumber = fileInfo[NSFileSize];
    NSUInteger fileSize = [fileSizeNumber unsignedIntegerValue];
    
    // on iOS7 we use the file directly
    NSInputStream *inputStream = [IPEnv isOs7] ? nil : [NSInputStream inputStreamWithFileAtPath:uploadTempFilename];
    
    // now create the actual request
    [self createRequestWithUrl:urlString];       // this sets urlRequest

    if (oAuthConsumer)                  // add the oauth header if needed
    {
        if (includeParamsInSignature)
            parametersForOAuth = inArguments;
        [self prepareOAuth];
    }
    
    urlRequest.timeoutInterval = defaultNetworkRequestUploadTimeout;
    [urlRequest setValue:contentType forHTTPHeaderField:@"Content-Type"];
    [urlRequest addValue:[@(fileSize) stringValue] forHTTPHeaderField: @"Content-Length"];
    [urlRequest setHTTPBodyStream:inputStream];
    
    return YES;
}

-(NSURL*)postBackgroundFilename
{
    if (!uploadTempFilename)
    {
        NSData *postData = [urlRequest HTTPBody];
        NSOutputStream *outStream = [self createTempFile];
        [outStream write:(uint8_t *)postData.bytes maxLength:postData.length];
        [outStream close];
    }
    return [[NSURL alloc] initFileURLWithPath:uploadTempFilename isDirectory:NO];
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// OAuth Stuff

static NSString *oAuthSignatureAlgorithm = @"HMAC-SHA1";

- (NSString *)generateSignatureBaseString:(NSDictionary*)userParameters timestamp:(NSString*)timestamp nonce:(NSString*)nonce
{
    // OAuth Spec, Section 9.1.1 "Normalize Request Parameters"
    // build a sorted array of both request parameters and OAuth header parameters
    
    NSMutableDictionary *allParameters = [[NSMutableDictionary alloc] initWithDictionary:userParameters];
    
    [allParameters addEntriesFromDictionary: @{ @"oauth_consumer_key":oAuthConsumer.key,    @"oauth_signature_method":oAuthSignatureAlgorithm,  @"oauth_timestamp":timestamp,
                                                @"oauth_nonce":nonce,                       @"oauth_version":@"1.0"} ];
    
    if (oAuthToken.key.length)
        allParameters[@"oauth_token"] = oAuthToken.key;
    
    NSMutableArray *parameterPairs = [[NSMutableArray alloc] initWithCapacity:allParameters.count]; // 6 being the number of OAuth params in the Signature Base String
    for (NSString *paramName in allParameters)
    {
        id paramValue = allParameters[paramName];
        if ([paramValue isKindOfClass:[NSArray class]])
        {
            for (id eachArrayValue in paramValue)
                [parameterPairs addObject:[NSString stringWithFormat:@"%@=%@",paramName.URLEncodedString, [eachArrayValue stringValue].URLEncodedStringForOAuthSignature]];
        }
        else
        {
            NSString *value = [paramValue stringValue];
            if (value)
            {
                [parameterPairs addObject:[NSString stringWithFormat:@"%@=%@", paramName, value.URLEncodedStringForOAuthSignature]];
            }
        }
    }
    
    NSArray *sortedPairs = [parameterPairs sortedArrayUsingSelector:@selector(compare:)];
    NSString *normalizedRequestParameters = [sortedPairs componentsJoinedByString:@"&"];
    
    // OAuth Spec, Section 9.1.2 "Concatenate Request Elements"
    NSString *urWithoutQuery = [self.urlString componentsSeparatedByString:@"?"][0];

    // Tumblr has this weird thing that ~ are not part of the oauth signature
    NSString *normalizedUrl = normalizedRequestParameters.URLEncodedString;
    if (self.specialOAuthEncoding)
        normalizedUrl =[normalizedUrl stringByReplacingOccurrencesOfString:@"%257E" withString:@"~"];
    
    NSString *signBaseString = [NSString stringWithFormat:@"%@&%@&%@", [urlRequest HTTPMethod], urWithoutQuery.URLEncodedString, normalizedUrl];
	
	return signBaseString;
}

- (void)prepareOAuth
{
    // sign : Secrets must be urlencoded before concatenated with '&'
    NSString *secret = [NSString stringWithFormat:@"%@&%@", [oAuthConsumer.secret URLEncodedString], [oAuthToken.secret URLEncodedString]];
    NSString *timestamp = [NSString stringWithFormat:@"%ld", time(NULL)];
    NSString *nonce = [self generateUniqueId];
    NSString *signBase = [self generateSignatureBaseString:parametersForOAuth timestamp:timestamp nonce:nonce];
    NSString *signature = [signBase hmacSign:secret];
    
    // set OAuth headers
    NSString *token;
    if (!oAuthToken.key.length)
        token = @""; // not used on Request Token transactions
    else
        token = [NSString stringWithFormat:@"oauth_token=\"%@\", ", oAuthToken.key.URLEncodedString];
	
    NSString *oauthHeader = [NSString stringWithFormat:@"OAuth realm=\"\", oauth_consumer_key=\"%@\", %@oauth_signature_method=\"%@\", oauth_signature=\"%@\", oauth_timestamp=\"%@\", oauth_nonce=\"%@\", oauth_version=\"1.0\"", oAuthConsumer.key.URLEncodedString, token, oAuthSignatureAlgorithm, signature.URLEncodedString, timestamp, nonce.URLEncodedString];
	
    [urlRequest setValue:oauthHeader forHTTPHeaderField:@"Authorization"];
}

- (NSString*)generateUniqueId
{
    CFUUIDRef theUUID = CFUUIDCreate(NULL);
    CFStringRef string = CFUUIDCreateString(NULL, theUUID);
    OSAtomicIncrement32(&signatureCount);
    NSString *nonce = [NSString stringWithFormat:@"%@-%d",(__bridge NSString*)string,(signatureCount % 1000)];  // add an extra sequence to the nonce
    CFRelease(string);
    CFRelease(theUUID);
    return nonce;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-(NSString*)description
{
    return [NSString stringWithFormat:@"<%@: %p> %@",NSStringFromClass([self class]), self, self.urlString];
}
-(NSURL*)url
{
    return urlRequest.URL;
}
-(NSString*)urlString
{
    return urlRequest.URL.absoluteString;
}
-(void)dealloc
{
    [self cancel];
}
@end



