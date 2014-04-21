#import "CNAtomic.h"

@implementation CNAtomicBool {
    int _value;
}
static ODClassType* _ATAtomicBool_type;

+ (id)atomicBool {
    return [[CNAtomicBool alloc] init];
}

- (id)init {
    self = [super init];

    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNAtomicBool class]) _ATAtomicBool_type = [ODClassType classTypeWithCls:[CNAtomicBool class]];
}

- (BOOL)boolValue {
    OSMemoryBarrier();
    return _value != 0;
}

- (void)setNewValue:(BOOL)newValue {
    OSMemoryBarrier();
    _value = newValue ? 1 : 0;
}

- (BOOL)getAndSetNewValue:(BOOL)newValue {
    for (;;) {
        OSMemoryBarrier();
        BOOL current = _value != 0;
        int e = current ? 1 : 0;
        int u = newValue ? 1 : 0;
        if (OSAtomicCompareAndSwap32(e, u, &_value))
            return current;
    }
    return NO;
}

- (BOOL)compareAndSetOldValue:(BOOL)oldValue newValue:(BOOL)newValue {
    int e = oldValue ? 1 : 0;
    int u = newValue ? 1 : 0;
    return OSAtomicCompareAndSwap32(e, u, &_value);
}

- (ODClassType*)type {
    return [CNAtomicBool type];
}

+ (ODClassType*)type {
    return _ATAtomicBool_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    OSMemoryBarrier();
    return _value == ((CNAtomicBool*)other)->_value;
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

@implementation CNAtomicInt {
    int _value;
}
static ODClassType* _ATAtomicInt_type;

+ (id)atomicInt {
    return [[CNAtomicInt alloc] init];
}

- (id)init {
    self = [super init];

    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNAtomicInt class]) _ATAtomicInt_type = [ODClassType classTypeWithCls:[CNAtomicInt class]];
}

- (int)intValue {
    OSMemoryBarrier();
    return _value;
}

- (void)setNewValue:(int)newValue {
    OSMemoryBarrier();
    _value = newValue;
}

- (int)incrementAndGet {
    return OSAtomicIncrement32Barrier(&_value);
}

- (int)decrementAndGet {
    return OSAtomicDecrement32Barrier(&_value);
}

- (int)addAndGetValue:(int)value {
    return OSAtomicAdd32(value, &_value);
}


- (BOOL)compareAndSetOldValue:(int)oldValue newValue:(int)newValue {
    return OSAtomicCompareAndSwap32(oldValue, newValue, &_value);
}

- (ODClassType*)type {
    return [CNAtomicBool type];
}

+ (ODClassType*)type {
    return _ATAtomicBool_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    OSMemoryBarrier();
    return _value == ((CNAtomicInt*)other)->_value;
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

@implementation CNAtomicObject {
    void* _value;
}
static ODClassType* _ATAtomicInt_type;

+ (instancetype)atomicObject {
    return [[CNAtomicObject alloc] init];
}

- (instancetype)init {
    self = [super init];

    return self;
}

- (instancetype)initWithValue:(id)value {
    self = [super init];
    if (self) {
        _value = (__bridge_retained void*)value;
    }

    return self;
}

+ (instancetype)atomicObjectWithValue:(id)value {
    return [((CNAtomicObject *)[self alloc]) initWithValue:value];
}


+ (void)initialize {
    [super initialize];
    if(self == [CNAtomicInt class]) _ATAtomicInt_type = [ODClassType classTypeWithCls:[CNAtomicInt class]];
}

- (id)value {
    OSMemoryBarrier();
    return (__bridge id)_value;
}

- (void)setNewValue:(id)newValue {
    OSMemoryBarrier();
    void *nv = (__bridge_retained void*)newValue;
    while(YES) {
        void *ov = _value;
        if(OSAtomicCompareAndSwapPtrBarrier(ov, nv, &_value)) {
            if(ov != nil) CFRelease(ov);
            return;
        }
    }
}

- (BOOL)compareAndSetOldValue:(id)oldValue newValue:(id)newValue {
    void *ov = (__bridge void*)oldValue;
    void *nv = (__bridge void*)newValue;
    if(OSAtomicCompareAndSwapPtrBarrier(ov, nv, &_value)) {
        if(ov != nil) CFRelease(ov);
        if(nv != nil) CFRetain(nv);
        return YES;
    }
    return NO;
}

- (ODClassType*)type {
    return [CNAtomicBool type];
}

+ (ODClassType*)type {
    return _ATAtomicBool_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    OSMemoryBarrier();
    return [((__bridge id)_value) isEqual:(__bridge id)((CNAtomicObject*)other)->_value];
}

- (NSUInteger)hash {
    return 0;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

- (void)dealloc {
    if(_value != nil) CFRelease(_value);
}


@end

