#import "objd.h"
#import "CNRange.h"

#import "ODType.h"
#import "CNChain.h"
#import "CNPlat.h"
#import "CNSet.h"
#import "CNDispatchQueue.h"
@implementation CNRange
static ODClassType* _CNRange_type;
@synthesize start = _start;
@synthesize end = _end;
@synthesize step = _step;
@synthesize count = _count;

+ (instancetype)rangeWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    return [[CNRange alloc] initWithStart:start end:end step:step];
}

- (instancetype)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    self = [super init];
    if(self) {
        _start = start;
        _end = end;
        _step = step;
        _count = ((step > 0) ? ((start <= end) ? ((NSUInteger)((end - start) / step + 1)) : 0) : ((step < 0) ? ((start >= end) ? ((NSUInteger)((end - start) / step + 1)) : 0) : 1));
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNRange class]) _CNRange_type = [ODClassType classTypeWithCls:[CNRange class]];
}

- (id)applyIndex:(NSUInteger)index {
    if(index < _count) return numi(_start + _step * index);
    else return nil;
}

- (id<CNIterator>)iterator {
    return [CNRangeIterator rangeIteratorWithStart:_start end:_end step:_step];
}

- (CNRange*)setStep:(NSInteger)step {
    return [CNRange rangeWithStart:_start end:_end step:step];
}

- (BOOL)isEmpty {
    if(_step > 0) {
        return _start > _end;
    } else {
        if(_step < 0) return _start < _end;
        else return NO;
    }
}

+ (CNRange*)applyI:(NSInteger)i {
    return [CNRange rangeWithStart:i end:i step:1];
}

- (id<CNImSeq>)addItem:(id)item {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
}

- (id<CNImSeq>)addSeq:(id<CNSeq>)seq {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendAllItems:seq];
    return [builder build];
}

- (id<CNImSeq>)subItem:(id)item {
    return [[[self chain] filter:^BOOL(id _) {
        return !([_ isEqual:item]);
    }] toArray];
}

- (id<CNMSeq>)mCopy {
    CNMArray* arr = [CNMArray array];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
}

- (id<CNSet>)toSet {
    return [self convertWithBuilder:[CNHashSetBuilder hashSetBuilder]];
}

- (BOOL)isEqualSeq:(id<CNSeq>)seq {
    if([self count] != [seq count]) return NO;
    id<CNIterator> ia = [self iterator];
    id<CNIterator> ib = [seq iterator];
    while([ia hasNext] && [ib hasNext]) {
        if(!([[ia next] isEqual:[ib next]])) return NO;
    }
    return YES;
}

- (id)head {
    return [self applyIndex:0];
}

- (id)last {
    return [self applyIndex:[self count] - 1];
}

- (id<CNImSeq>)tail {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    id<CNIterator> i = [self iterator];
    if([i hasNext]) {
        [i next];
        while([i hasNext]) {
            [builder appendItem:[i next]];
        }
    }
    return [builder build];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
    }
}

- (void)parForEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        id v = [i next];
        [CNDispatchQueue.aDefault asyncF:^void() {
            each(v);
        }];
    }
}

- (BOOL)goOn:(BOOL(^)(id))on {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if(!(on([i next]))) return NO;
    }
    return YES;
}

- (BOOL)containsItem:(id)item {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if([[i next] isEqual:i]) return YES;
    }
    return NO;
}

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = nil;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = x;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = YES;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    __block BOOL ret = YES;
    [self goOn:^BOOL(id x) {
        if(!(confirm(x))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
}

- (ODClassType*)type {
    return [CNRange type];
}

+ (ODClassType*)type {
    return _CNRange_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other)) return NO;
    if([other conformsToProtocol:@protocol(CNSeq)]) return [self isEqualSeq:((id<CNSeq>)(other))];
    return NO;
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + self.start;
    hash = hash * 31 + self.end;
    hash = hash * 31 + self.step;
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"start=%ld", (long)self.start];
    [description appendFormat:@", end=%ld", (long)self.end];
    [description appendFormat:@", step=%ld", (long)self.step];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNRangeIterator
static ODClassType* _CNRangeIterator_type;
@synthesize start = _start;
@synthesize end = _end;
@synthesize step = _step;

+ (instancetype)rangeIteratorWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    return [[CNRangeIterator alloc] initWithStart:start end:end step:step];
}

- (instancetype)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    self = [super init];
    if(self) {
        _start = start;
        _end = end;
        _step = step;
        _i = start;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNRangeIterator class]) _CNRangeIterator_type = [ODClassType classTypeWithCls:[CNRangeIterator class]];
}

- (BOOL)hasNext {
    return (_step > 0 && _i <= _end) || (_step < 0 && _i >= _end);
}

- (id)next {
    NSInteger ret = _i;
    _i += _step;
    return numi(ret);
}

- (ODClassType*)type {
    return [CNRangeIterator type];
}

+ (ODClassType*)type {
    return _CNRangeIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"start=%ld", (long)self.start];
    [description appendFormat:@", end=%ld", (long)self.end];
    [description appendFormat:@", step=%ld", (long)self.step];
    [description appendString:@">"];
    return description;
}

@end


