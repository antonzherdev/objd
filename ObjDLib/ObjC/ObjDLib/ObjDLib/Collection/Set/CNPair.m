#import "objd.h"
#import "CNPair.h"

#import "CNType.h"
@implementation CNPair
static CNClassType* _CNPair_type;
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
    if(self == [CNPair class]) _CNPair_type = [CNClassType classTypeWithCls:[CNPair class]];
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

- (BOOL)isEqualPair:(CNPair*)pair {
    return ({
        id __tmp__il_aitem = pair.a;
        [_a isEqual:__tmp__il_aitem] || [_b isEqual:__tmp__il_aitem];
    }) && ({
        id __tmp__il_bitem = pair.b;
        [_a isEqual:__tmp__il_bitem] || [_b isEqual:__tmp__il_bitem];
    });
}

- (CNClassType*)type {
    return [CNPair type];
}

+ (CNClassType*)type {
    return _CNPair_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other)) return NO;
    if([other isKindOfClass:[CNPair class]]) return [self isEqualPair:((CNPair*)(other))];
    return NO;
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
static CNClassType* _CNPairIterator_type;
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
    if(self == [CNPairIterator class]) _CNPairIterator_type = [CNClassType classTypeWithCls:[CNPairIterator class]];
}

- (BOOL)hasNext {
    return _state < 2;
}

- (id)next {
    _state++;
    if(_state == 1) return _pair.a;
    else return _pair.b;
}

- (CNClassType*)type {
    return [CNPairIterator type];
}

+ (CNClassType*)type {
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

