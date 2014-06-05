//
// Created by Anton Zherdev on 27.12.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSDate (CNChain)
- (NSTimeInterval)sinceNow;

- (NSTimeInterval)tillNow;
@end