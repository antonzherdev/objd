#import "CNTypes.h"
#import "CNZipLink.h"
#import "CNCollection.h"


@implementation CNZipLink {
    id<CNIterable> _a;
    cnF2 _f;
}
- (id)initWithA:(id <CNIterable>)a by:(cnF2)f {
    self = [super init];
    if (self) {
        _a = a;
        _f=f;
    }

    return self;
}

+ (id)linkWithA:(id <CNIterable>)a by:(cnF2)f {
    return [[self alloc] initWithA:a by:f];
}


- (CNYield *)buildYield:(CNYield *)yield {
    id <CNIterator> a = [_a iterator];
    return [CNYield decorateYield:yield begin:nil yield:^CNYieldResult(id item) {
        if(!a.hasNext) return cnYieldBreak;
        return [yield yieldItem:_f(item, a.next)];
    } end:nil all:nil];
}

@end

@implementation CNZip3Link {
    id<CNIterable> _a;
    id<CNIterable> _b;
    cnF3 _f;
}
- (id)initWithA:(id <CNIterable>)a b:(id <CNIterable>)b by:(cnF3)f {
    self = [super init];
    if (self) {
        _a = a;
        _f=f;
        _b=b;
    }

    return self;
}

+ (id)linkWithA:(id <CNIterable>)a b:(id <CNIterable>)b by:(cnF3)f {
    return [[self alloc] initWithA:a b:b by:f];
}


- (CNYield *)buildYield:(CNYield *)yield {
    id <CNIterator> a = [_a iterator];
    id <CNIterator> b = [_b iterator];
    return [CNYield decorateYield:yield begin:nil yield:^CNYieldResult(id item) {
        if(!a.hasNext) return cnYieldBreak;
        if(!b.hasNext) return cnYieldBreak;
        return [yield yieldItem:_f(item, a.next, b.next)];
    } end:nil all:nil];
}

@end