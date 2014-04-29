//
// Created by Anton Zherdev on 12.12.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNTopLink : NSObject <CNChainLink>
- (instancetype)initWithNumbers:(NSUInteger)numbers;

+ (instancetype)linkWithNumbers:(NSUInteger)numbers;


@end