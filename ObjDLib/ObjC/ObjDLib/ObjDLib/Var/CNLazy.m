#import "objd.h"
#import "CNLazy.h"

#import "ODType.h"
@implementation CNLazy
static ODClassType* _CNLazy_type;
@synthesize f = _f;

+ (instancetype)lazyWithF:(id(^)())f {
    return [[CNLazy alloc] initWithF:f];
}

- (instancetype)initWithF:(id(^)())f {
    self = [super init];
    if(self) {
        _f = [f copy];
        __calculated = NO;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNLazy class]) _CNLazy_type = [ODClassType classTypeWithCls:[CNLazy class]];
}

- (BOOL)isCalculated {
    return __calculated;
}

- (id)get {
    if(__calculated) {
        return ((id)(__value));
    } else {
        __value = _f();
        __calculated = YES;
        return ((id)(__value));
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNCache
static ODClassType* _CNCache_type;
@synthesize f = _f;

+ (instancetype)cacheWithF:(id(^)(id))f {
    return [[CNCache alloc] initWithF:f];
}

- (instancetype)initWithF:(id(^)(id))f {
    self = [super init];
    if(self) _f = [f copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNCache class]) _CNCache_type = [ODClassType classTypeWithCls:[CNCache class]];
}

- (id)applyX:(id)x {
    if(__lastX != nil && [__lastX isEqual:x]) {
        return ((id)(__lastF));
    } else {
        __lastX = x;
        __lastF = _f(x);
        return ((id)(__lastF));
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNWeak
static ODClassType* _CNWeak_type;
@synthesize value = _value;

+ (instancetype)weakWithValue:(id)value {
    return [[CNWeak alloc] initWithValue:value];
}

- (instancetype)initWithValue:(id)value {
    self = [super init];
    if(self) _value = value;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNWeak class]) _CNWeak_type = [ODClassType classTypeWithCls:[CNWeak class]];
}

- (BOOL)isEmpty {
    return _value == nil;
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"value=%@", self.value];
    [description appendString:@">"];
    return description;
}

@end


