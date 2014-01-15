#import "objd.h"
#import "CNData.h"

#import "CNTypes.h"
#import "ODType.h"
#import "CNSet.h"
#import "CNChain.h"
@implementation CNPArray{
    NSUInteger _stride;
    id(^_wrap)(VoidRef, NSUInteger);
    NSUInteger _count;
    NSUInteger _length;
    VoidRef _bytes;
    BOOL _copied;
}
static ODClassType* _CNPArray_type;
@synthesize stride = _stride;
@synthesize wrap = _wrap;
@synthesize count = _count;
@synthesize length = _length;
@synthesize bytes = _bytes;
@synthesize copied = _copied;

+ (id)arrayWithStride:(NSUInteger)stride wrap:(id(^)(VoidRef, NSUInteger))wrap count:(NSUInteger)count length:(NSUInteger)length bytes:(VoidRef)bytes copied:(BOOL)copied {
    return [[CNPArray alloc] initWithStride:stride wrap:wrap count:count length:length bytes:bytes copied:copied];
}

- (id)initWithStride:(NSUInteger)stride wrap:(id(^)(VoidRef, NSUInteger))wrap count:(NSUInteger)count length:(NSUInteger)length bytes:(VoidRef)bytes copied:(BOOL)copied {
    self = [super init];
    if(self) {
        _stride = stride;
        _wrap = wrap;
        _count = count;
        _length = length;
        _bytes = bytes;
        _copied = copied;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNPArray_type = [ODClassType classTypeWithCls:[CNPArray class]];
}

+ (CNPArray*)applyStride:(NSUInteger)stride wrap:(id(^)(VoidRef, NSUInteger))wrap count:(NSUInteger)count copyBytes:(VoidRef)copyBytes {
    NSUInteger len = count * stride;
    return [CNPArray arrayWithStride:stride wrap:wrap count:count length:len bytes:copy(copyBytes, count * stride) copied:YES];
}

- (id<CNIterator>)iterator {
    return [CNPArrayIterator arrayIteratorWithArray:self];
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= _count) return nil;
    else return _wrap(_bytes, index);
}

- (void)dealloc {
    if(_copied) free(_bytes);
}

- (id)unsafeApplyIndex:(NSUInteger)index {
    return _wrap(_bytes, index);
}

- (void)forRefEach:(void(^)(VoidRef))each {
    VoidRef b = _bytes;
    NSInteger i = 0;
    while(i < _count) {
        each(b);
        i++;
        b = b + _stride;
    }
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
    return [CNPArray type];
}

+ (ODClassType*)type {
    return _CNPArray_type;
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


@implementation CNPArrayIterator{
    CNPArray* _array;
    NSInteger _i;
}
static ODClassType* _CNPArrayIterator_type;
@synthesize array = _array;

+ (id)arrayIteratorWithArray:(CNPArray*)array {
    return [[CNPArrayIterator alloc] initWithArray:array];
}

- (id)initWithArray:(CNPArray*)array {
    self = [super init];
    if(self) {
        _array = array;
        _i = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNPArrayIterator_type = [ODClassType classTypeWithCls:[CNPArrayIterator class]];
}

- (BOOL)hasNext {
    return _i < _array.count;
}

- (id)next {
    id ret = [_array unsafeApplyIndex:((NSUInteger)(_i))];
    _i++;
    return ret;
}

- (ODClassType*)type {
    return [CNPArrayIterator type];
}

+ (ODClassType*)type {
    return _CNPArrayIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNPArrayIterator* o = ((CNPArrayIterator*)(other));
    return [self.array isEqual:o.array];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.array hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"array=%@", self.array];
    [description appendString:@">"];
    return description;
}

@end


