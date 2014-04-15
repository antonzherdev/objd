#import "CNAppendLink.h"
#import "CNCollection.h"


@implementation CNAppendLink {
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
        return [yield beginYieldWithSize:size + [_collection count]];

    }                      yield:nil end:^CNYieldResult(CNYieldResult r) {
        __block CNYieldResult result = r;
        if (result != cnYieldBreak) {
            [_collection goOn:^BOOL(id item) {
                result = [yield yieldItem:item];
                if (result == cnYieldBreak) return NO;
                return YES;
            }];
        }
        return [yield endYieldWithResult:result];
    }                        all:nil];
}

+ (id)linkWithCollection:(id)collection {
    return [[self alloc] initWithCollection:collection];
}

@end