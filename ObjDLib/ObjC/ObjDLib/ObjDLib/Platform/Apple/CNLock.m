//
// Created by Anton Zherdev on 28/04/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import "CNLock.h"
#import "NSConditionLock+CNChain.h"


@implementation CNLock {
    NSConditionLock *_lock;
    int _conditionCounter;
}
+ (CNLock *)lock {
    return [[CNLock alloc] init];
}

- (id)init {
    self = [super init];
    if (self) {
        _lock = [[NSConditionLock alloc] initWithCondition:0];
    }

    return self;
}


- (void)lock {
    [_lock lock];
}

- (CNLockCondition *)newCondition {
    _conditionCounter++;
    return [[CNLockCondition alloc] initWithLock:_lock number:_conditionCounter];
}

- (void)unlock {
    [_lock unlock];
}
@end

@implementation CNLockCondition {
    int _number;
    NSConditionLock* _lock;
}

- (id)initWithLock:(NSConditionLock *)lock number:(int)number {
    self = [super init];
    if (self) {
        _number = number;
        _lock=lock;
    }

    return self;
}


- (BOOL)awaitPeriod:(CGFloat)period {
    [_lock unlock];
    BOOL lock = [_lock lockWhenCondition:_number period:period];
    if(!lock) [_lock lock];
    return lock;
}

- (void)signal {
    [_lock unlockWithCondition:_number];
    [_lock lock];
}

- (void)await {
    [_lock unlock];
    [_lock lockWhenCondition:_number];
}

- (void)unlockedSignal {
    [_lock lock];
    [_lock unlockWithCondition:_number];
}

- (BOOL)unlockedAwaitPeriod:(CGFloat)period {
    BOOL lock = [_lock lockWhenCondition:_number period:period];
    if(lock) {
        [_lock unlock];
        return YES;
    } else {
        return NO;
    }
}

- (void)unlockedAwait {
    [_lock lockWhenCondition:_number];
    [_lock unlock];
}
@end