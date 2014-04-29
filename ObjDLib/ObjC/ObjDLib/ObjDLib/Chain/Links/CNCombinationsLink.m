#import "CNTypes.h"
#import "CNCombinationsLink.h"
#import "CNTuple.h"


@implementation CNCombinationsLink

- (id)init{
    self = [super init];

    return self;
}

+ (id)link {
    return [[self alloc] init];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block NSMutableArray * previous = nil;
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        previous = [NSMutableArray arrayWithCapacity:size];
        return [yield beginYieldWithSize:size <= 1 ? 0 : size - 1];
    }                      yield:^int(id item) {
        for (id prev in previous) {
            if ([yield yieldItem:tuple(prev, item)] == 1) return 1;
        }
        [previous addObject:item];
        return 0;
    }                        end:nil all:nil];
}

@end