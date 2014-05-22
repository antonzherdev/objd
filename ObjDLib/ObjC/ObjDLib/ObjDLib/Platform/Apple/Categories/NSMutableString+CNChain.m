//
// Created by Anton Zherdev on 22/05/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import "NSMutableString+CNChain.h"


@implementation NSMutableString (CNChain)
+ (NSMutableString*) stringBuilder {
    return [NSMutableString string];
}

- (void)appendStr:(NSString *)str {
    [self appendString:str];
}

- (void)appendObj:(NSObject *)str {
    [self appendString:[str description]];
}

- (void)appendItem:(id)item {
    [self appendFormat:@"%c", [item charValue]];
}

- (void)appendAllItems:(id <CNTraversable>)items {
    if([items isKindOfClass:[NSString class]]) {
        [self appendString:(NSString*) items];
    } else {
        [items forEach:^(id o) {
            [self appendFormat:@"%c", [o charValue]];
        }];
    }
}

- (NSString *)build {
    return self;
}

@end