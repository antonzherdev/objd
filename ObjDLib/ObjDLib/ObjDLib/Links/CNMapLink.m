#import "CNTypes.h"
#import "CNMapLink.h"


@implementation CNMapLink {
    cnF _f;
}
- (id)initWithF:(cnF)f {
    self = [super init];
    if (self) {
        _f = [f copy];
    }

    return self;
}

+ (id)linkWithF:(cnF)f {
    return [[self alloc] initWithF:f];
}


- (CNYield *)buildYield:(CNYield *)yield {
    return [CNYield decorateYield:yield begin:nil yield:^CNYieldResult(id item) {
        return [yield yieldItem:_f(item)];
    } end:nil all:nil];
}

@end