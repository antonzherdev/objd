#import "objd.h"
#import "CNLazy.h"

#import "CNType.h"
#import "CNString.h"
@implementation CNLazy
static CNClassType* _CNLazy_type;
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
    if(self == [CNLazy class]) _CNLazy_type = [CNClassType classTypeWithCls:[CNLazy class]];
}

- (BOOL)isCalculated {
    return __calculated;
}

- (id)get {
    if(__calculated) {
        return __value;
    } else {
        __value = _f();
        __calculated = YES;
        return __value;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@")"];
}

- (CNClassType*)type {
    return [CNLazy type];
}

+ (CNClassType*)type {
    return _CNLazy_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNCache
static CNClassType* _CNCache_type;
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
    if(self == [CNCache class]) _CNCache_type = [CNClassType classTypeWithCls:[CNCache class]];
}

- (id)applyX:(id)x {
    if(__lastX != nil && [__lastX isEqual:x]) {
        return __lastF;
    } else {
        __lastX = x;
        __lastF = _f(x);
        return __lastF;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@")"];
}

- (CNClassType*)type {
    return [CNCache type];
}

+ (CNClassType*)type {
    return _CNCache_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNWeak
static CNClassType* _CNWeak_type;
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
    if(self == [CNWeak class]) _CNWeak_type = [CNClassType classTypeWithCls:[CNWeak class]];
}

- (BOOL)isEmpty {
    return _value == nil;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Weak(%@)", _value];
}

- (CNClassType*)type {
    return [CNWeak type];
}

+ (CNClassType*)type {
    return _CNWeak_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

