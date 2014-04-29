//
// Created by Anton Zherdev on 21.08.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import "CNPlatform.h"


@implementation OSBundle {

}
+ (NSString *)readToStringResource:(NSString *)resource {
    NSString* file = [OSBundle fileNameForResource:resource];
    NSError* error;
    NSString *string = [NSString stringWithContentsOfFile:file encoding:NSUTF8StringEncoding error:&error];
    if(!string) {
        @throw error;
    }
    return string;
}

+ (NSString *)fileNameForResource:(NSString *)resource {
    return [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:resource];
}
@end

@implementation OSDirectory
+ (NSString *)sandbox {
    return NSHomeDirectory();
}
@end


@implementation OSLocale
+ (NSArray *)preferredLanguages {
    return [NSLocale preferredLanguages];
}

@end