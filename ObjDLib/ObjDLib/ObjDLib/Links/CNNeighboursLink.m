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
    return [CNYield decorateYield:yield begin:^CNYieldResult(NSUInteger size) {
        return [yield beginYieldWithSize:size <= 1 ? 0 : size - 1];
    } yield: ^CNYieldResult(id item) {
        CNYieldResult result = cnYieldContinue;
        if(prev != nil) result = [yield yieldItem:tuple(prev, item)];
        prev = item;
        if(first == nil) first = item;
        return result;
    } end:_ring ? ^CNYieldResult(CNYieldResult result) {
        if(result == cnYieldContinue) result = [yield yieldItem:tuple(prev, first)];
        return [yield endYieldWithResult:result];
    } : nil all:nil];
}

@end