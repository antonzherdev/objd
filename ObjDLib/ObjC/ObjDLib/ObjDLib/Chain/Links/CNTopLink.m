//
// Created by Anton Zherdev on 12.12.13.
// Copyright (c) 2013 Anton Zherdev. All rights reserved.
//

#import "CNTopLink.h"


@implementation CNTopLink {
    NSUInteger _numbers;

}
- (instancetype)initWithNumbers:(NSUInteger)numbers {
    self = [super init];
    if (self) {
        _numbers = numbers;
    }

    return self;
}

+ (instancetype)linkWithNumbers:(NSUInteger)numbers {
    return [[self alloc] initWithNumbers:numbers];
}

- (CNYield *)buildYield:(CNYield *)yield {
    __block NSUInteger n = 0;
    return [CNYield decorateBase:yield begin:nil yield:^int(id item) {
        if (_numbers == 0) return 1;
        int result = [yield yieldItem:item];
        n++;
        return (n >= _numbers) ? 1 : result;
    }                        end:nil all:nil];
}

@end