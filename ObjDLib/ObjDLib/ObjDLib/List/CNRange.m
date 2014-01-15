#import "objd.h"
#import "CNRange.h"

#import "ODType.h"
#import "CNSet.h"
#import "CNChain.h"
@implementation CNRange{
    NSInteger _start;
    NSInteger _end;
    NSInteger _step;
    NSUInteger _count;
}
static ODClassType* _CNRange_type;
@synthesize start = _start;
@synthesize end = _end;
@synthesize step = _step;
@synthesize count = _count;

+ (id)rangeWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    return [[CNRange alloc] initWithStart:start end:end step:step];
}

- (id)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    self = [super init];
    if(self) {
        _start = start;
        _end = end;
        _step = step;
        _count = ((_step > 0) ? ((_start <= _end) ? ((NSUInteger)((_end - _start) / _step + 1)) : 0) : ((_step < 0) ? ((_start >= _end) ? ((NSUInteger)((_end - _start) / _step + 1)) : 0) : 1));
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNRange_type = [ODClassType classTypeWithCls:[CNRange class]];
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= _count) @throw @"Incorrect index";
    else return numi(_start + _step * index);
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

- (id)optIndex:(NSUInteger)index {
    if(index >= [self count]) return [CNOption none];
    else return [CNOption applyValue:[self applyIndex:index]];
}

- (id)randomItem {
    NSUInteger c = [self count];
    if(c == 0) {
        return [CNOption none];
    } else {
        if(c == 1) return [CNOption applyValue:[self head]];
        else return [CNOption applyValue:[self applyIndex:oduIntRndMax([self count] - 1)]];
    }
}

- (id<CNSet>)toSet {
    return [self convertWithBuilder:[CNHashSetBuilder hashSetBuilder]];
}

- (id<CNSeq>)addItem:(id)item {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
}

- (id<CNSeq>)addSeq:(id<CNSeq>)seq {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendAllItems:seq];
    return [builder build];
}

- (id<CNSeq>)subItem:(id)item {
    return [[[self chain] filter:^BOOL(id _) {
        return !([_ isEqual:item]);
    }] toArray];
}

- (BOOL)isEqualToSeq:(id<CNSeq>)seq {
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

- (id)headOpt {
    return [self optIndex:0];
}

- (id<CNSeq>)tail {
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

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
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

- (NSString*)description {
    return [[self chain] toStringWithStart:@"[" delimiter:@", " end:@"]"];
}

- (NSUInteger)hash {
    NSUInteger ret = 13;
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        ret = ret * 31 + [[i next] hash];
    }
    return ret;
}

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = [CNOption none];
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = [CNOption applyValue:x];
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
    if([other conformsToProtocol:@protocol(CNSeq)]) return [self isEqualToSeq:((id<CNSeq>)(other))];
    return NO;
}

@end


@implementation CNRangeIterator{
    NSInteger _start;
    NSInteger _end;
    NSInteger _step;
    NSInteger _i;
}
static ODClassType* _CNRangeIterator_type;
@synthesize start = _start;
@synthesize end = _end;
@synthesize step = _step;

+ (id)rangeIteratorWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    return [[CNRangeIterator alloc] initWithStart:start end:end step:step];
}

- (id)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    self = [super init];
    if(self) {
        _start = start;
        _end = end;
        _step = step;
        _i = _start;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNRangeIterator_type = [ODClassType classTypeWithCls:[CNRangeIterator class]];
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

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNRangeIterator* o = ((CNRangeIterator*)(other));
    return self.start == o.start && self.end == o.end && self.step == o.step;
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


