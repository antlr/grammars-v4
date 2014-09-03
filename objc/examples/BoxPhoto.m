//
//  BoxPhoto.m
//  FlickStackr
//
//  Created by Carlos Mejia on 2012-09-20.
//  Copyright (c) 2012 iPont. All rights reserved.
//

#import "BoxPhoto.h"
#import "BoxSession.h"
#import "BoxPhotoInfo.h"
#import "DateConversion.h"
#import "BoxPhotoSource.h"
#import "BoxPhotoInfo.h"

@interface BoxPhoto()
{
    BOOL generatedGoodThumbs;
}
@end
@implementation BoxPhoto

static NSSet *s_allowedExtensions;
+(void)initialize
{
    if (!s_allowedExtensions)
    {
        s_allowedExtensions  = [[NSSet alloc] initWithArray:@[@"JPEG", @"JPG",@"PNG",@"GIF", @"MOV", @"MP4",@"TIF",@"TIFF", @"BMP" ]];
    }
}

-(NSString*)stringValue:(NSDictionary*)photo name:(NSString*)name
{
    id rawValue = photo[name];
    return ([rawValue isKindOfClass:[NSDictionary class]]) ? rawValue[TXT] : (   ([rawValue isKindOfClass:[NSNull class]]) ? nil : [rawValue stringValue]  );
}

-(instancetype)initUserInfoWithDictionary:(NSDictionary*)photo box:(BoxSession*)box
{
    if (self = [super initEmpty])
    {
        @try
        {
            NSString *userId = [self stringValue:photo name:@"id"];
            self.pid = userId;
            
            self.title = [self stringValue:photo name:@"name"];
            NSString *buddyUrl = [self stringValue:photo name:@"avatar_url"];
            
            [self setImagePath:PLTHUMB url:buddyUrl];
            [self setImagePath:PLSQUARE url:buddyUrl];
            self.index = 0;
        }
        @catch (NSException *exception)
        {
            NSLog(@"EX(BoxPhoto init) %@",exception);
        }
	}
	return self;
}

-(NSString*)createThumbnailPhotoUrl:(NSString*)imageSize box:(BoxSession*)box
{
    NSString *small = [NSString stringWithFormat:@"https://api.box.com/2.0/files/%@/thumbnail.png?min_height=%@&min_width=%@$%@$", [self.pid URLEncodedStringExceptSlash], imageSize, imageSize, box.sessionKey];
    return small;
}
-(NSString*)createPhotoUrl:(BoxSession*)box
{
    NSString *small = [NSString stringWithFormat:@"https://api.box.com/2.0/files/%@/content$%@$", [self.pid URLEncodedStringExceptSlash], box.sessionKey];
    return small;
}
-(instancetype)initWithDictionary:(NSDictionary*)photo index:(NSInteger)indexInSet box:(BoxSession*)box
{
    if (self = [super initEmpty])
    {
        @try
        {
            NSString *fileName = [self stringValue:photo name:@"name"];
            NSString *upperFileExt = [fileName.uppercaseString pathExtension];
            if (upperFileExt.length && ![s_allowedExtensions containsObject:upperFileExt])
                return nil;

            NSString *fileId = [self stringValue:photo name:@"id"];

            if (!fileId)
                fileId = [self stringValue:photo name:@"file_id"];
            self.pid = fileId;
            self.title = [self stringValue:photo name:@"name"];
            self.visibility = PhotoVisiblePrivate;
            self.folderId = [photo[@"parent"][@"id"] stringValue];
            
            BOOL isVideo = [upperFileExt isEqualToString:@"MOV"] || [upperFileExt isEqualToString:@"MP4"];
            
            self.index = indexInSet;
            if (!self.title.length)
                self.title = fileName;
            NSDictionary *ownerInfo = photo[@"owned_by"];
            self.ownerId = [ownerInfo[@"id"] stringValue];
            self.ownerName = ownerInfo[@"name"];
            self.caption = photo[@"description"];
            self.visibility = PhotoVisiblePrivate;;
            self.dateUploaded = [DateConversion  parseYmdhmsTZTZDate:photo[@"created_at"]];
            self.byteSize = [[self stringValue:photo name:@"size"] longLongValue];
            self.mediaType = (isVideo) ? PhotoMediaVideo : PhotoMediaPhoto;
            self.permissions = [BoxPhoto boxPermissionFromDictionary:photo[@"permissions"]];
            NSDictionary *sharedLink = photo[@"shared_link"];
            if (sharedLink && ![sharedLink isKindOfClass:[NSNull class]])
            {
                self.shareLink = sharedLink[@"url"];
                self.visibility = PhotoVisiblePublic;
            }
            
            [self setImagePathsFromDictionary:photo box:box];
        }
        @catch (NSException *exception)
        {
            NSLog(@"EX(BoxPhoto init) %@",exception);
        }
	}
	return self;
}
-(void)setImagePathsFromDictionary:(NSDictionary*)photo  box:(BoxSession*)box
{
    NSString *original = [self createPhotoUrl:box];
    if (self.mediaType == PhotoMediaVideo)
    {
        NSString *videoImage = @"page_white_film";
        [self setImagePath:PLSQUARE url:videoImage];
    }
    else
    {
        NSString *thumb = [self createThumbnailPhotoUrl:@"128" box:box];
        NSString *small = [self createThumbnailPhotoUrl:@"256" box:box];
        NSString *medium = [self createThumbnailPhotoUrl:@"256" box:box];
        
        [self setImagePath:PLSQUARE url:thumb];
        [self setImagePath:PLTHUMB url:thumb];
        [self setImagePath:PLSMALL url:small];
        [self setImagePath:PLMEDIUM url:medium];
        [self setImagePath:PLLARGE url:original]; // regular large image
    }
    [self setImagePath:PLORIGINAL url:original];
}


- (void)encodeWithCoder:(NSCoder *)aCoder
{
    @try
    {
        [super encodeWithCoder:aCoder];
        [aCoder encodeInt32:self.permissions forKey:kn_permissions];
    }
    @catch (NSException *exception)
    {
        NSLog(@"Problem saving photo : %@,%@",self.pid,exception);
    }
}

- (instancetype)initWithCoder:(NSCoder *)aDecoder
{
	if (self=[super initWithCoder:aDecoder])
	{
        @try
        {
            self.permissions = [aDecoder decodeObjectForKey:kn_permissions];
        }
        @catch (NSException *exception)
        {
            NSLog(@"Problem loading photo : %@,%@",self.pid,exception);
        }
	}
	return self;
}

- (id)copyWithZone:(NSZone *)zone
{
	BoxPhoto *n = (BoxPhoto *) [super copyWithZone:zone];
    n.permissions = self.permissions;
	return n;
}
-(void)updatePhotoUrls:(NSDictionary*)urlDictionary  session:(ProviderSession*)session
{
    [self removeImagesFromCache];
    [self setImagePathsFromDictionary:urlDictionary box:(BoxSession*)session];
    self.providerPhotoSource.saved = NO;
}
-(ProviderPhotoInfo*)photoInfo:(ProviderSession *)session
{
    ProviderPhotoInfo *created = [super photoInfo:session];
    BoxPhotoInfo *boxInfo = SAFE_CAST(created, BoxPhotoInfo);
    boxInfo.isFolder = self.container;
    return created;
}

-(BOOL)canShareThumbnail
{
    return NO;
}

-(NSURL*)getBrowserUrl:(ProviderSession *)flickrSession
{
    return [NSURL URLWithString:[NSString stringWithFormat:@"https://www.box.com/files#/files/0/f/%@/1/f_%@",self.folderId,self.pid]];
}

// overriden for folders
-(NSString*)shareLinkRequestPath
{
    return [NSString stringWithFormat:@"files/%@", self.pid];;
}

-(void)generateShareLink:(BoxSession*)session delegate:(id<FSFilePhotoDelegate>)delegate
{
    NSString *path = [self shareLinkRequestPath];
    __weak id<FSFilePhotoDelegate> linkDelegate = delegate;
    
    [session requestPutJSONOAuth2WithPath:path params:@{@"shared_link":@{@"access":@"open"}} response:^(NSDictionary *response, NSError *error)
    {
        if (!error)
        {
            NSString *link= response[@"shared_link"][@"url"];
            [self updateReceivedShareLink:link updateVisibility:YES];
        }
        runOnMainThread(^
        {
            [linkDelegate linkReceived:self error:error];
        });
    }];
}

-(UIImage*)getCalculatedImage:(PhotoSize)photoSize targetSize:(CGFloat)targetSize
{
    if (self.mediaType == PhotoMediaVideo)
        return [super generateStaticFolderImage:photoSize targetSize:targetSize];
    return nil;
}

-(UIImage*)getOverlayImage:(ProviderPhotoSource*)source
{
	if ([self isNotAvailable:source])
	{
		return [super getOverlayImage:source];
	}
	return nil;
}
-(void)getVideoUrl:(ProviderSession *)boxSession  liveView:(BOOL)liveView downloadSize:(PhotoSize)downloadSize  videoBlock:(VideoUrlBlock)videoUrlBlock
{
    NSString *videoPath = [self getRemoteImagePath:PLORIGINAL];
    NSArray *splitVideoUrl = [videoPath componentsSeparatedByString:@"$"];  // remove the session ID
    NSString* imageRequestUrl = splitVideoUrl[0];  // real URL
    
    if (liveView)
    {
        NSString *localUrl = [[FSPhotoCache sharedCache] filenameForCachedURL:videoPath  extension:self.fileExtension fullPath:YES];
        if (localUrl)
        {
            // we have it locally
            videoUrlBlock([NSURL fileURLWithPath:localUrl], nil, videoPath);
        }
        else
        {
            // for box we need to create a request for the video, but tell NetworkRequest to ignore
            // the redirect, so that we get a temp usable URL for streaming
            VideoUrlBlock videoLocalBlock = [videoUrlBlock copy];
            
            [boxSession createAuthenticatedURLRequestForPhoto:imageRequestUrl executeRequest:^(NetworkRequest *networkRequest)
            {
                networkRequest.ignoreRedirect = YES;
                [networkRequest openWithStringResponse:^(NSString *response, NSError *error)
                {
                    // response contains the temporary URL
                    videoLocalBlock([NSURL URLWithString:response], nil, videoPath);
                }];
            }];
        }
    }
    else
    {
        VideoUrlBlock videoLocalBlock = [videoUrlBlock copy];
        [boxSession createAuthenticatedURLRequestForPhoto:imageRequestUrl executeRequest:^(NetworkRequest *networkRequest)
        {
             videoLocalBlock(nil,networkRequest, videoPath);
        }];
    }
}

-(BOOL)canModify:(ProviderSession *)session
{
    return [self isMine:session] || (self.permissions & BoxPermissionRename);
}
-(BoxSession*)photoSession
{
    ProviderSession *ses = self.providerPhotoSource.session;
    return SAFE_CAST(ses,BoxSession);
}

-(BOOL)updateRemoteImagePaths:(FSPhotoUpdateUrlBlock)inUpdateUrlBlock
{
    BoxSession *session = self.photoSession;
    if (!session)
        return NO;
    FSPhotoUpdateUrlBlock updateUrlBlock = [inUpdateUrlBlock copy];
    
    NSString *path = [NSString stringWithFormat:@"files/%@",self.pid];
    
    [session requestOAuth2WithPath:path params:nil response:^(NSDictionary *response, NSError *error)
     {
         id<FSPhoto> updatedPhoto;
         if (!error)
         {
             [self setImagePathsFromDictionary:response box:session];
             self.providerPhotoSource.saved = NO;  // save the new copy
             updatedPhoto = self;
         }
         
         if (updateUrlBlock)
             updateUrlBlock(updatedPhoto);
         
     }];
    return YES;
}

-(UIImage*)visibilityImage
{
    return (self.visibilityForDisplay == PhotoVisiblePublic) ? [UIImage imageNamed:@"linkshared"] : nil;
}

+(BoxPermissions)boxPermissionFromDictionary:(NSDictionary*)dict
{
    BoxPermissions perm = BoxPermissionUnknown;
    if (dict.count)
    {
        perm |= ([dict[@"can_delete"] boolValue]) ? BoxPermissionDelete : 0;
        perm |= ([dict[@"can_download"] boolValue]) ? BoxPermissionDownload : 0;
        perm |= ([dict[@"can_invite_collaborator"] boolValue]) ? BoxPermissionInvite : 0;
        perm |= ([dict[@"can_rename"] boolValue]) ? BoxPermissionRename : 0;
        perm |= ([dict[@"can_set_share_access"] boolValue]) ? BoxPermissionShareAccess : 0;
        perm |= ([dict[@"can_share"] boolValue]) ? BoxPermissionShare : 0;
        perm |= ([dict[@"can_upload"] boolValue]) ? BoxPermissionUpload : 0;
    }
    return perm;
}
@end
