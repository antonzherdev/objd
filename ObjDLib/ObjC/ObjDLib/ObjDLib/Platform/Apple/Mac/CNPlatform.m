//
// Created by Anton Zherdev on 21.08.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import "CNPlatform.h"


@implementation CNBundle {

}
+ (NSString *)readToStringResource:(NSString *)resource {
    NSString* file = [CNBundle fileNameForResource:resource];
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

@implementation CNDirectory
+ (NSString *)sandbox {
    return NSHomeDirectory();
}
@end


@implementation CNLocale
+ (NSArray *)preferredLanguages {
    return [NSLocale preferredLanguages];
}

@end