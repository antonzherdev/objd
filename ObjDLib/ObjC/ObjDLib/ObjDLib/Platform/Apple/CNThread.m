//
// Created by Anton Zherdev on 27/02/14.
// Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

#import "CNThread.h"


@implementation CNThread {

}
+ (void)sleepPeriod:(CGFloat)d {
    [NSThread sleepForTimeInterval:d];
}
@end