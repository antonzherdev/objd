#import "CNTypes.h"
#import "CNFlatMapLink.h"
#import "CNChain.h"


@implementation CNFlatMapLink {
    double _factor;
    cnF _f;
}
- (id)initWithF:(cnF)f factor:(double)factor {
    self = [super init];
    if (self) {
        _f = [f copy];
        _factor = factor;
    }

    return self;
}

+ (id)linkWithF:(cnF)f factor:(double)factor {
    return [[self alloc] initWithF:f factor:factor];
}


- (CNYield *)buildYield:(CNYield *)yield {
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        return [yield beginYieldWithSize:(NSUInteger) (size * _factor)];
    }                      yield:^int(id item) {
        id o = _f(item);
        if ([o isKindOfClass:[CNChain class]]) {
            __block int ret = 0;
            [((CNChain *) o) apply:[CNYield yieldWithBegin:nil yield:^int(id x) {
                ret = [yield yieldItem:x];
                return ret;
            }                                          end:nil all:nil]];
            return ret;
        } else {
            __block int result = 0;
            [o goOn:^BOOL(id x) {
                result = [yield yieldItem:x];
                if (result == 1) return NO;
                return YES;
            }];
            return result;
        }

    }                        end:nil all:nil];
}

@end