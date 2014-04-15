#import "objd.h"
#import "CNSeq.h"

#import "CNSet.h"
#import "ODType.h"
#import "CNDispatchQueue.h"
#import "CNChain.h"
@implementation CNArrayBuilder
static ODClassType* _CNArrayBuilder_type;

+ (instancetype)arrayBuilder {
    return [[CNArrayBuilder alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _array = [NSMutableArray mutableArray];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNArrayBuilder class]) _CNArrayBuilder_type = [ODClassType classTypeWithCls:[CNArrayBuilder class]];
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNIndexFunSeq
static ODClassType* _CNIndexFunSeq_type;
@synthesize count = _count;
@synthesize f = _f;

+ (instancetype)indexFunSeqWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    return [[CNIndexFunSeq alloc] initWithCount:count f:f];
}

- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    self = [super init];
    if(self) {
        _count = count;
        _f = [f copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNIndexFunSeq class]) _CNIndexFunSeq_type = [ODClassType classTypeWithCls:[CNIndexFunSeq class]];
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= _count) return nil;
    else return _f(index);
}

- (id<CNIterator>)iterator {
    return [CNIndexFunSeqIterator indexFunSeqIteratorWithCount:_count f:_f];
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
    NSMutableArray* arr = [NSMutableArray mutableArray];
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

- (BOOL)isEmpty {
    return [self count] == 0;
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
    if([other conformsToProtocol:@protocol(CNSeq)]) return [self isEqualSeq:((id<CNSeq>)(other))];
    return NO;
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


@implementation CNIndexFunSeqIterator
static ODClassType* _CNIndexFunSeqIterator_type;
@synthesize count = _count;
@synthesize f = _f;

+ (instancetype)indexFunSeqIteratorWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    return [[CNIndexFunSeqIterator alloc] initWithCount:count f:f];
}

- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    self = [super init];
    if(self) {
        _count = count;
        _f = [f copy];
        _i = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNIndexFunSeqIterator class]) _CNIndexFunSeqIterator_type = [ODClassType classTypeWithCls:[CNIndexFunSeqIterator class]];
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"count=%lu", (unsigned long)self.count];
    [description appendString:@">"];
    return description;
}

@end


