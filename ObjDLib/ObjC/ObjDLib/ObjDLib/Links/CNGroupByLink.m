#import "CNTypes.h"
#import "CNGroupByLink.h"
#import "CNTuple.h"
#import "NSMutableDictionary+CNChain.h"


@implementation CNGroupByLink {
    cnF _by;
    cnF0 _start;
    cnF2 _fold;
    double _factor;
    BOOL _mutableMode;
    cnF _mapAfter;
}

- (id)initWithBy:(cnF)by fold:(cnF2)fold withStart:(cnF0)start factor:(double)factor mutableMode:(BOOL)mutableMode mapAfter:(cnF)mapAfter {
    self = [super init];
    if (self) {
        _by = by;
        _fold = fold;
        _start = start;
        _factor = factor;
        _mutableMode = mutableMode;
        _mapAfter = mapAfter;
    }

    return self;
}

+ (id)linkWithBy:(cnF)by fold:(cnF2)fold withStart:(cnF0)start factor:(double)factor mutableMode:(BOOL)mutableMode mapAfter:(cnF)mapAfter {
    return [[self alloc] initWithBy:by fold:fold withStart:start factor:factor mutableMode:mutableMode mapAfter:mapAfter];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block NSMutableDictionary * dictionary = nil;
    return [CNYield decorateBase:yield begin:^CNYieldResult(NSUInteger size) {
        NSUInteger newSize = (NSUInteger) (size * _factor);
        dictionary = [NSMutableDictionary dictionaryWithCapacity:newSize];
        return [yield beginYieldWithSize:newSize];
    }                      yield:^CNYieldResult(id item) {
        id key = _by(item);
        id r = [dictionary objectForKey:key];
        if (r == nil) {
            r = _start();
            if (_mutableMode) [dictionary setObject:r forKey:key];
        }
        r = _fold(r, item);
        if (!_mutableMode) [dictionary setObject:r forKey:key];
        return cnYieldContinue;
    }                        end:^CNYieldResult(CNYieldResult result) {
        if (result != cnYieldBreak) {
            if (_mapAfter != nil) {
                __block CNYieldResult r = cnYieldContinue;
                [dictionary enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
                    if ([yield yieldItem:tuple(key, _mapAfter(obj))] == cnYieldBreak) {
                        r = cnYieldBreak;
                        *stop = YES;
                    }
                }];
                result = r;
            } else {
                result = [yield yieldAll:dictionary];
            }
        }
        return [yield endYieldWithResult:result];
    }                        all:nil];
}

@end