#import "CNTypes.h"
#import "CNFilterLink.h"


@implementation CNFilterLink {
    cnPredicate _predicate;
    double _selectivity;
}
- (id)initWithPredicate:(cnPredicate)predicate selectivity:(double)selectivity {
    self = [super init];
    if (self) {
        _predicate = [predicate copy];
        _selectivity = selectivity;
    }

    return self;
}

- (CNYield *)buildYield:(CNYield *)yield {
    return [CNYield decorateYield:yield begin:^CNYieldResult(NSUInteger size) {
        return [yield beginYieldWithSize:(NSUInteger) (size * _selectivity)];
    } yield:^CNYieldResult(id item) {
        if(!_predicate(item)) return cnYieldContinue;
        return [yield yieldItem:item];
    } end:nil all:nil];
}

+ (id)linkWithPredicate:(cnPredicate)predicate selectivity:(double)selectivity {
    return [[self alloc] initWithPredicate:predicate selectivity:0];
}

@end