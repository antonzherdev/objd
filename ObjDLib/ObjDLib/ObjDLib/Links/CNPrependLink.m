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
    return [CNYield decorateBase:yield begin:^CNYieldResult(NSUInteger size) {
        if ([yield beginYieldWithSize:size + [_collection count]] == cnYieldBreak) return cnYieldBreak;
        return [_collection goOn:^BOOL(id item) {
            return [yield yieldItem:item] == cnYieldContinue;
        }] ? cnYieldContinue : cnYieldBreak;
    }                      yield:nil end:nil all:nil];
}

+ (id)linkWithCollection:(id)collection {
    return [[self alloc] initWithCollection:collection];
}

@end