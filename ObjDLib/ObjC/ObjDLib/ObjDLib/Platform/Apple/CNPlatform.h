//
// Created by Anton Zherdev on 21.08.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import <Foundation/Foundation.h>


@interface OSBundle : NSObject
+ (NSString *)readToStringResource:(NSString *)resource;

+ (NSString *)fileNameForResource:(NSString *)resource;
@end

@interface OSDirectory : NSObject
+ (NSString*) sandbox;
@end

@interface OSLocale : NSObject
+ (NSArray*) preferredLanguages;
@end