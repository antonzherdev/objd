#import "objd.h"
#import "CNPair.h"

#import "ODType.h"
#import "CNPlat.h"
#import "CNDispatchQueue.h"
#import "CNChain.h"
@implementation CNPair
static ODClassType* _CNPair_type;
@synthesize a = _a;
@synthesize b = _b;

+ (instancetype)pairWithA:(id)a b:(id)b {
    return [[CNPair alloc] initWithA:a b:b];
}

- (instancetype)initWithA:(id)a b:(id)b {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNPair class]) _CNPair_type = [ODClassType classTypeWithCls:[CNPair class]];
}

+ (CNPair*)newWithA:(id)a b:(id)b {
    if(a < b) return [CNPair pairWithA:a b:b];
    else return [CNPair pairWithA:b b:a];
}

- (BOOL)containsItem:(id)item {
    return [_a isEqual:item] || [_b isEqual:item];
}

- (NSUInteger)count {
    return 2;
}

- (id<CNIterator>)iterator {
    return [CNPairIterator pairIteratorWithPair:self];
}

- (id)head {
    return _a;
}

- (id<CNMSet>)mCopy {
    CNMHashSet* arr = [CNMHashSet hashSet];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
}

- (BOOL)isEmpty {
    return !([[self iterator] hasNext]);
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
    return [CNPair type];
}

+ (ODClassType*)type {
    return _CNPair_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNPair* o = ((CNPair*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"a=%@", self.a];
    [description appendFormat:@", b=%@", self.b];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNPairIterator
static ODClassType* _CNPairIterator_type;
@synthesize pair = _pair;

+ (instancetype)pairIteratorWithPair:(CNPair*)pair {
    return [[CNPairIterator alloc] initWithPair:pair];
}

- (instancetype)initWithPair:(CNPair*)pair {
    self = [super init];
    if(self) {
        _pair = pair;
        _state = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNPairIterator class]) _CNPairIterator_type = [ODClassType classTypeWithCls:[CNPairIterator class]];
}

- (BOOL)hasNext {
    return _state < 2;
}

- (id)next {
    _state++;
    if(_state == 1) return _pair.a;
    else return _pair.b;
}

- (ODClassType*)type {
    return [CNPairIterator type];
}

+ (ODClassType*)type {
    return _CNPairIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"pair=%@", self.pair];
    [description appendString:@">"];
    return description;
}

@end


