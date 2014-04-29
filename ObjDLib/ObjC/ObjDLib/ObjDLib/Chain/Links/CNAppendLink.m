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
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        return [yield beginYieldWithSize:size + [_collection count]];

    }                      yield:nil end:^int(int r) {
        __block int result = r;
        if (result != 1) {
            [_collection goOn:^BOOL(id item) {
                result = [yield yieldItem:item];
                if (result == 1) return NO;
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