#import "CNMulLink.h"
#import "CNTuple.h"
#import "CNCollection.h"


@implementation CNMulLink {
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
    return [CNYield decorateYield: yield begin:^CNYieldResult(NSUInteger size) {
        return [yield beginYieldWithSize:size*[_collection count]];

    } yield:^CNYieldResult(id a) {
        return [_collection goOn:^BOOL(id b) {
            CNTuple *item = [CNTuple tupleWithA:a b: b];
            return [yield yieldItem:item] == cnYieldContinue;
        }] ? cnYieldContinue : cnYieldBreak;
    } end:nil all:nil];
}

+ (id)linkWithCollection:(id)collection {
    return [[self alloc] initWithCollection:collection];
}

@end