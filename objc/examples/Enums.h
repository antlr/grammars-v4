#import <Foundation/Foundation.h>

typedef enum : NSUInteger {
    PINSpeedRecorderConnectionStatusNotReachable,
    PINSpeedRecorderConnectionStatusWWAN,
    PINSpeedRecorderConnectionStatusWiFi
} PINSpeedRecorderConnectionStatus;

typedef NS_ENUM(NSInteger, XYAttachmentType) {
    XYAttachmentTypeOne = 0,
    XYAttachmentTypeTwo = 1, 
};