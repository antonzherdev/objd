//
// Created by Anton Zherdev on 28/04/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import <Foundation/Foundation.h>

@class CNLockCondition;


@interface CNLock : NSObject
+ (CNLock *)lock;
- (void)lock;

- (CNLockCondition *)newCondition;

- (void)unlock;
@end

@interface CNLockCondition : NSObject
- (id)initWithLock:(NSConditionLock *)lock number:(NSInteger)number;

- (void)awaitPeriod:(CGFloat)period;

- (void)signal;

- (void)await;

- (void)unlockedSignal;

- (void)unlockedAwaitPeriod:(CGFloat)period;

- (void)unlockedAwait;
@end