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
    return [CNYield yieldWithBegin:nil yield:nil end:^int(int result) {
        if (result == 0) {
            [yield yieldAllItems:_collection];
        }
        return 0;
    } all:nil];
}

+ (id)linkWithCollection:(id)collection {
    return [[self alloc] initWithCollection:collection];
}

@end