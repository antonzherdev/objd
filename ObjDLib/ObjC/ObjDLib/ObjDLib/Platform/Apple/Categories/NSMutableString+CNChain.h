//
// Created by Anton Zherdev on 22/05/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "CNCollection.h"

@interface NSMutableString (CNChain) <CNBuilder>
+ (NSMutableString*) stringBuilder;
- (void)appendStr:(NSString*)str;
- (void)appendObj:(NSObject*)str;
- (NSString*)build;

- (void)appendCh:(unichar)item;
@end