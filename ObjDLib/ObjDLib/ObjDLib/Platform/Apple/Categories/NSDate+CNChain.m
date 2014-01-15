//
// Created by Anton Zherdev on 27.12.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//

#import "NSDate+CNChain.h"


@implementation NSDate (CNChain)
- (NSTimeInterval)sinceNow {
    return [self timeIntervalSinceNow];
}

- (NSTimeInterval)beforeNow {
    return -[self timeIntervalSinceNow];
}
@end