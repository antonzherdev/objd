#import "CNReverseLink.h"
#import "CNDistinctLink.h"


@implementation CNDistinctLink {
    double _selectivity;
}


- (id)initWithSelectivity:(double)selectivity {
    self = [super init];
    if (self) {
        _selectivity = selectivity;
    }

    return self;
}

+ (id)linkWithSelectivity:(double)selectivity {
    return [[self alloc] initWithSelectivity:selectivity];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block NSMutableSet* set;
    return [CNYield decorateYield: yield begin:^CNYieldResult(NSUInteger size) {
        NSUInteger newSize = (NSUInteger) (size * _selectivity);
        set = [NSMutableSet setWithCapacity:newSize];
        return [yield beginYieldWithSize:newSize];
    } yield:^CNYieldResult(id item) {
        if([set containsObject:item]) return cnYieldContinue;
        [set addObject:item];
        return [yield yieldItem:item];
    } end: nil all:nil];
}

+ (id)link {
    return [[self alloc] init];
}

@end