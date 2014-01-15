#import "objd.h"
#import "CNSeq.h"

#import "CNSet.h"
#import "CNChain.h"
#import "ODType.h"
@implementation CNArrayBuilder{
    NSMutableArray* _array;
}
static ODClassType* _CNArrayBuilder_type;
@synthesize array = _array;

+ (id)arrayBuilder {
    return [[CNArrayBuilder alloc] init];
}

- (id)init {
    self = [super init];
    if(self) _array = [NSMutableArray mutableArray];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNArrayBuilder_type = [ODClassType classTypeWithCls:[CNArrayBuilder class]];
}

- (void)appendItem:(id)item {
    [_array appendItem:item];
}

- (NSArray*)build {
    return _array;
}

- (void)appendAllItems:(id<CNTraversable>)items {
    [items forEach:^void(id _) {
        [self appendItem:_];
    }];
}

- (ODClassType*)type {
    return [CNArrayBuilder type];
}

+ (ODClassType*)type {
    return _CNArrayBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    return YES;
}

- (NSUInteger)hash {
    return 0;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNIndexFunSeq{
    NSUInteger _count;
    id(^_f)(NSUInteger);
}
static ODClassType* _CNIndexFunSeq_type;
@synthesize count = _count;
@synthesize f = _f;

+ (id)indexFunSeqWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    return [[CNIndexFunSeq alloc] initWithCount:count f:f];
}

- (id)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    self = [super init];
    if(self) {
        _count = count;
        _f = f;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNIndexFunSeq_type = [ODClassType classTypeWithCls:[CNIndexFunSeq class]];
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= _count) @throw @"Incorrect index";
    else return _f(index);
}

- (id<CNIterator>)iterator {
    return [CNIndexFunSeqIterator indexFunSeqIteratorWithCount:_count f:_f];
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

- (BOOL)isEmpty {
    return [self count] == 0;
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
    return [CNIndexFunSeq type];
}

+ (ODClassType*)type {
    return _CNIndexFunSeq_type;
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


@implementation CNIndexFunSeqIterator{
    NSUInteger _count;
    id(^_f)(NSUInteger);
    NSUInteger _i;
}
static ODClassType* _CNIndexFunSeqIterator_type;
@synthesize count = _count;
@synthesize f = _f;
@synthesize i = _i;

+ (id)indexFunSeqIteratorWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    return [[CNIndexFunSeqIterator alloc] initWithCount:count f:f];
}

- (id)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    self = [super init];
    if(self) {
        _count = count;
        _f = f;
        _i = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNIndexFunSeqIterator_type = [ODClassType classTypeWithCls:[CNIndexFunSeqIterator class]];
}

- (BOOL)hasNext {
    return _i < _count;
}

- (id)next {
    id ret = _f(_i);
    _i++;
    return ret;
}

- (ODClassType*)type {
    return [CNIndexFunSeqIterator type];
}

+ (ODClassType*)type {
    return _CNIndexFunSeqIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNIndexFunSeqIterator* o = ((CNIndexFunSeqIterator*)(other));
    return self.count == o.count && [self.f isEqual:o.f];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + self.count;
    hash = hash * 31 + [self.f hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"count=%lu", (unsigned long)self.count];
    [description appendString:@">"];
    return description;
}

@end


