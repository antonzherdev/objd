#import "CNSortLink.h"
#import "CNTreeSet.h"


@implementation CNSortLink {
   cnCompare _comparator;
}
- (id)init {
    self = [super init];
    if (self) {

    }

    return self;
}

- (id)initWithComparator:(cnCompare)comparator {
    self = [super init];
    if (self) {
        _comparator = ^NSInteger(id x, id y) {
            NSInteger ret = comparator(x, y);
            return ret == 0 ? 1 : ret;
        };
    }

    return self;
}

+ (id)linkWithComparator:(cnCompare)comparator {
    return [[self alloc] initWithComparator:comparator];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block CNMTreeSet* ret = [CNMTreeSet applyComparator:_comparator];
    return [CNYield decorateBase:yield begin:nil yield:^CNYieldResult(id item) {
        [ret appendItem:item];
        return cnYieldContinue;
    }                        end:^CNYieldResult(CNYieldResult result) {
        if (result != cnYieldBreak) {
            [yield yieldAll:ret];
        }
        return [yield endYieldWithResult:result];
    }                        all:nil];
}

+ (id)link {
    return [[self alloc] init];
}

@end