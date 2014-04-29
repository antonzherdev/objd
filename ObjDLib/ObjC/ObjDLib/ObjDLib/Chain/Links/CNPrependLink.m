#import "CNPrependLink.h"
#import "CNCollection.h"


@implementation CNPrependLink {
    id _collection;
}
- (id)initWithCollection:(id)collection {
    self = [super init];
    if (self) {
        _collection = cnResolveCollection(collection);
    }

    return self;
}

- (CNYield *)buildYield:(CNYield *)yield {
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        if ([yield beginYieldWithSize:size + [_collection count]] == 1) return 1;
        return [_collection goOn:^BOOL(id item) {
            return [yield yieldItem:item] == 0;
        }] ? 0 : 1;
    }                      yield:nil end:nil all:nil];
}

+ (id)linkWithCollection:(id)collection {
    return [[self alloc] initWithCollection:collection];
}

@end