#import "CNTypes.h"
#import "CNUncombinationsLink.h"
#import "CNTuple.h"


@implementation CNUncombinationsLink

- (id)init{
    self = [super init];

    return self;
}

+ (id)link {
    return [[self alloc] init];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block NSMutableSet * previous = nil;
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        previous = [NSMutableSet setWithCapacity:size + 1];
        return [yield beginYieldWithSize:size + 1];
    }                      yield:^int(id item) {
        int ret = 0;
        id a = [item a];
        if (![previous containsObject:a]) {
            ret = [yield yieldItem:a];
            [previous addObject:a];
        }
        if (ret == 0) {
            id b = [item b];
            if (![previous containsObject:b]) {
                ret = [yield yieldItem:b];
                [previous addObject:b];
            }
        }

        return ret;
    }                        end:nil all:nil];
}

@end