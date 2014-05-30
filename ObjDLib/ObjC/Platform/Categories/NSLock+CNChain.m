//
// Created by Anton Zherdev on 25/02/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import "NSLock+CNChain.h"


@implementation NSLock (CNChain)
+ (instancetype) lock {
    return [[NSLock alloc] init];
}
@end