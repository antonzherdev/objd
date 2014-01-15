#import "CNSourceLink.h"


@implementation CNSourceLink {
    id _collection;
}
- (id)initWithCollection:(id)collection {
    self = [super init];
    if (self) {
        _collection = collection;
    }

    return self;
}

- (CNYield *)buildYield:(CNYield *)yield {
    return [CNYield yieldWithBegin:nil yield:nil end:^CNYieldResult(CNYieldResult result) {
        if (result == cnYieldContinue) {
            [yield yieldAll:_collection];
        }
        return cnYieldContinue;
    } all:nil];
}

+ (id)linkWithCollection:(id)collection {
    return [[self alloc] initWithCollection:collection];
}

@end