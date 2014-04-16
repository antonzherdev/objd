#import "ODType.h"

@implementation ODType

+ (id)type {
    return [[ODType alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

- (Class)cls {
    @throw @"Method cls is abstract";
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

- (BOOL)isEqualToOther:(ODType*)other {
    return self == other;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    return self == other;
}
@end


@implementation ODClassType{
    Class _cls;
}
static ODClassType* _ODClassType_type;
@synthesize cls = _cls;

+ (id)classTypeWithCls:(Class)cls {
    return [[ODClassType alloc] initWithCls:cls];
}

- (id)initWithCls:(Class)cls {
    self = [super init];
    if(self) _cls = cls;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _ODClassType_type = [ODClassType classTypeWithCls:[ODClassType class]];
}

- (NSString*)name {
    return NSStringFromClass(_cls);
}

- (ODClassType*)type {
    return [ODClassType type];
}

+ (ODClassType*)type {
    return _ODClassType_type;
}
@end


@implementation ODPType{
    Class _cls;
    NSString* _name;
    NSUInteger _size;
    id(^_wrap)(void*, NSUInteger);
}
static ODClassType* _ODPType_type;
@synthesize cls = _cls;
@synthesize name = _name;
@synthesize size = _size;
@synthesize wrap = _wrap;

+ (id)typeWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap {
    return [[ODPType alloc] initWithCls:cls name:name size:size wrap:wrap];
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
    _ODPType_type = [ODClassType classTypeWithCls:[ODPType class]];
}

- (ODClassType*)type {
    return [ODPType type];
}

+ (ODClassType*)type {
    return _ODPType_type;
}
@end


