#import "CNTypes.h"
#import "CNNeighboursLink.h"
#import "CNTuple.h"


@implementation CNNeighboursLink {
    BOOL _ring;
}

- (id)initWithRing:(BOOL)ring {
    self = [super init];
    if (self) {
        _ring = ring;
    }

    return self;
}

+ (id)linkWithRing:(BOOL)ring {
    return [[self alloc] initWithRing:ring];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block id first = nil;
    __block id prev = nil;
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        return [yield beginYieldWithSize:size <= 1 ? 0 : size - 1];
    }                      yield:^int(id item) {
        int result = 0;
        if (prev != nil) result = [yield yieldItem:tuple(prev, item)];
        prev = item;
        if (first == nil) first = item;
        return result;
    }                        end:_ring ? ^int(int result) {
        if (result == 0) result = [yield yieldItem:tuple(prev, first)];
        return [yield endYieldWithResult:result];
    } : nil                  all:nil];
}

@end