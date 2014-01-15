#import "objd.h"
#import "CNLazy.h"

#import "ODType.h"
@implementation CNLazy{
    id(^_f)();
    id __value;
    BOOL __calculated;
}
static ODClassType* _CNLazy_type;
@synthesize f = _f;

+ (id)lazyWithF:(id(^)())f {
    return [[CNLazy alloc] initWithF:f];
}

- (id)initWithF:(id(^)())f {
    self = [super init];
    if(self) {
        _f = f;
        __calculated = NO;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNLazy_type = [ODClassType classTypeWithCls:[CNLazy class]];
}

- (BOOL)isCalculated {
    return __calculated;
}

- (id)get {
    if(__calculated) {
        return __value;
    } else {
        __value = ((id(^)())(_f))();
        __calculated = YES;
        return __value;
    }
}

- (ODClassType*)type {
    return [CNLazy type];
}

+ (ODClassType*)type {
    return _CNLazy_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNLazy* o = ((CNLazy*)(other));
    return [self.f isEqual:o.f];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.f hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNCache{
    id(^_f)(id);
    id __lastX;
    id __lastF;
}
static ODClassType* _CNCache_type;
@synthesize f = _f;

+ (id)cacheWithF:(id(^)(id))f {
    return [[CNCache alloc] initWithF:f];
}

- (id)initWithF:(id(^)(id))f {
    self = [super init];
    if(self) _f = f;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNCache_type = [ODClassType classTypeWithCls:[CNCache class]];
}

- (id)applyX:(id)x {
    if([x isEqual:__lastX]) {
        return __lastF;
    } else {
        __lastX = x;
        __lastF = _f(x);
        return __lastF;
    }
}

- (ODClassType*)type {
    return [CNCache type];
}

+ (ODClassType*)type {
    return _CNCache_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNCache* o = ((CNCache*)(other));
    return [self.f isEqual:o.f];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.f hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNWeak{
    __weak id _get;
}
static ODClassType* _CNWeak_type;
@synthesize get = _get;

+ (id)weakWithGet:(id)get {
    return [[CNWeak alloc] initWithGet:get];
}

- (id)initWithGet:(id)get {
    self = [super init];
    if(self) _get = get;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNWeak_type = [ODClassType classTypeWithCls:[CNWeak class]];
}

- (ODClassType*)type {
    return [CNWeak type];
}

+ (ODClassType*)type {
    return _CNWeak_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNWeak* o = ((CNWeak*)(other));
    return [self.get isEqual:o.get];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.get hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"get=%@", self.get];
    [description appendString:@">"];
    return description;
}

@end


