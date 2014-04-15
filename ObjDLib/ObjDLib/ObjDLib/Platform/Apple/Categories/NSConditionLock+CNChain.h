//
// Created by Anton Zherdev on 25/02/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSConditionLock (CNChain)
+ (NSConditionLock *)conditionLockWithCondition:(int)i;

- (BOOL)lockWhenCondition:(int)i period:(CGFloat)period;
@end