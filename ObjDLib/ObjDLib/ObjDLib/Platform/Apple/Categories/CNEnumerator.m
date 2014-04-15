#import "CNEnumerator.h"
#import "CNTypes.h"


@implementation CNEnumerator {
    NSEnumerator * _enumerator;
    id _next;
    BOOL _calledHasNext;
}
- (id)initWithEnumerator:(NSEnumerator *)enumerator {
    self = [super init];
    if (self) {
        _enumerator = enumerator;
    }

    return self;
}

- (BOOL)hasNext {
    if(!_calledHasNext) {
        _next = uwrapNil([_enumerator nextObject]);
        _calledHasNext = YES;
    }
    return _next != nil;
}

- (id)next {
    if(_calledHasNext) {
        _calledHasNext = NO;
        id ret = _next;
        _next = nil;
        return ret;
    }
    return uwrapNil([_enumerator nextObject]);
}

+ (id)enumeratorWithEnumerator:(NSEnumerator *)enumerator {
    return [[self alloc] initWithEnumerator:enumerator];
}

@end

@implementation CNMEnumerator {
    NSEnumerator * _enumerator;
    id _next;
    BOOL _calledHasNext;
}
- (id)initWithEnumerator:(NSEnumerator *)enumerator {
    self = [super init];
    if (self) {
        _enumerator = enumerator;
    }

    return self;
}

- (BOOL)hasNext {
    if(!_calledHasNext) {
        _next = uwrapNil([_enumerator nextObject]);
        _calledHasNext = YES;
    }
    return _next != nil;
}

- (id)next {
    if(_calledHasNext) {
        _calledHasNext = NO;
        id ret = _next;
        _next = nil;
        return ret;
    }
    return uwrapNil([_enumerator nextObject]);
}
- (void)setValue:(id)value {
    @throw @"Hasn't implemented yet";
}

- (void)remove {
    @throw @"Hasn't implemented yet";
}

+ (id)enumeratorWithEnumerator:(NSEnumerator *)enumerator {
    return [[self alloc] initWithEnumerator:enumerator];
}

@end