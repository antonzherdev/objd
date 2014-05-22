#import "CNType.h"

@implementation CNType

+ (id)type {
    return [[CNType alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

- (NSString*)name {
    @throw @"Method name is abstract";
}

- (NSString*)description {
    return [[self name] stringByAppendingString:@".type"];
}

- (NSUInteger)hash {
    return (NSUInteger) self;
}

- (BOOL)isEqualToOther:(CNType *)other {
    return self == other;
}

- (BOOL)isInstanceObj:(NSObject *)obj {
    return NO;
}


- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    return self == other;
}
@end


@implementation CNClassType {
    Class _cls;
}
static CNClassType * _ODClassType_type;
@synthesize cls = _cls;

+ (id)classTypeWithCls:(Class)cls {
    return [[CNClassType alloc] initWithCls:cls];
}

- (id)initWithCls:(Class)cls {
    self = [super init];
    if(self) _cls = cls;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _ODClassType_type = [CNClassType classTypeWithCls:[CNClassType class]];
}

- (NSString*)name {
    return NSStringFromClass(_cls);
}

- (CNClassType *)type {
    return [CNClassType type];
}

+ (CNClassType *)type {
    return _ODClassType_type;
}

- (BOOL)isInstanceObj:(NSObject *)obj {
    return [obj isKindOfClass:_cls];
}

@end

@implementation CNTraitType {
    Protocol * _cls;
    NSString* _name;
}
static CNClassType * _ODProtocolType_type;
@synthesize cls = _cls;

+ (id)classTypeWithCls:(Protocol*)cls name:(NSString*)name{
    return [[CNTraitType alloc] initWithCls:cls name:name];
}

- (id)initWithCls:(Protocol*)cls name:(NSString*)name{
    self = [super init];
    if(self) {
        _cls = cls;
        _name = name;
    }

    return self;
}

+ (void)initialize {
    [super initialize];
    _ODProtocolType_type = [CNClassType classTypeWithCls:[CNTraitType class]];
}

- (NSString*)name {
    return _name;
}

- (CNClassType *)type {
    return [CNTraitType type];
}

+ (CNClassType *)type {
    return _ODProtocolType_type;
}

- (BOOL)isInstanceObj:(NSObject *)obj {
    return [obj conformsToProtocol:_cls];
}
@end


@implementation CNPType {
    Class _cls;
    NSString* _name;
    NSUInteger _size;
    id(^_wrap)(void*, NSUInteger);
}
static CNClassType * _ODPType_type;
@synthesize cls = _cls;
@synthesize name = _name;
@synthesize size = _size;
@synthesize wrap = _wrap;

+ (id)typeWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap {
    return [[CNPType alloc] initWithCls:cls name:name size:size wrap:wrap];
}

- (id)initWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap {
    self = [super init];
    if(self) {
        _cls = cls;
        _name = name;
        _size = size;
        _wrap = wrap;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _ODPType_type = [CNClassType classTypeWithCls:[CNPType class]];
}

- (CNClassType *)type {
    return [CNPType type];
}

+ (CNClassType *)type {
    return _ODPType_type;
}

- (BOOL)isInstanceObj:(NSObject *)obj {
    return [obj isKindOfClass:_cls];
}
@end


