#import "CNTypes.h"
#import "CNCombinationsLink.h"
#import "CNTuple.h"


@implementation CNCombinationsLink

- (id)init{
    self = [super init];

    return self;
}

+ (id)link {
    return [[self alloc] init];
}


- (CNYield *)buildYield:(CNYield *)yield {
    __block NSMutableArray * previous = nil;
    return [CNYield decorateYield:yield begin:^CNYieldResult(NSUInteger size) {
        previous = [NSMutableArray arrayWithCapacity:size];
        return [yield beginYieldWithSize:size <= 1 ? 0 : size - 1];
    } yield: ^CNYieldResult(id item) {
        for(id prev in previous) {
            if([yield yieldItem:tuple(prev, item)] == cnYieldBreak) return cnYieldBreak;
        }
        [previous addObject:item];
        return cnYieldContinue;
    } end: nil all:nil];
}

@end