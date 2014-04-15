#import "objd.h"
#import "CNTry.h"

#import "ODType.h"
@implementation CNTry
static ODClassType* _CNTry_type;

+ (instancetype)try {
    return [[CNTry alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTry class]) _CNTry_type = [ODClassType classTypeWithCls:[CNTry class]];
}

- (id)get {
    @throw @"Method get is abstract";
}

- (id)reason {
    @throw @"Method reason is abstract";
}

- (BOOL)isSuccess {
    @throw @"Method isSuccess is abstract";
}

- (BOOL)isFailure {
    return !([self isSuccess]);
}

- (CNTry*)mapF:(id(^)(id))f {
    @throw @"Method map is abstract";
}

- (ODClassType*)type {
    return [CNTry type];
}

+ (ODClassType*)type {
    return _CNTry_type;
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


@implementation CNSuccess
static ODClassType* _CNSuccess_type;
@synthesize get = _get;

+ (instancetype)successWithGet:(id)get {
    return [[CNSuccess alloc] initWithGet:get];
}

- (instancetype)initWithGet:(id)get {
    self = [super init];
    if(self) _get = get;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSuccess class]) _CNSuccess_type = [ODClassType classTypeWithCls:[CNSuccess class]];
}

- (BOOL)isSuccess {
    return YES;
}

- (BOOL)isFailure {
    return NO;
}

- (id)reason {
    @throw @"Getting reason for success try";
}

- (CNTry*)mapF:(id(^)(id))f {
    return [CNSuccess successWithGet:f(_get)];
}

- (ODClassType*)type {
    return [CNSuccess type];
}

+ (ODClassType*)type {
    return _CNSuccess_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"get=%@", self.get];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNFailure
static ODClassType* _CNFailure_type;
@synthesize reason = _reason;

+ (instancetype)failureWithReason:(id)reason {
    return [[CNFailure alloc] initWithReason:reason];
}

- (instancetype)initWithReason:(id)reason {
    self = [super init];
    if(self) _reason = reason;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFailure class]) _CNFailure_type = [ODClassType classTypeWithCls:[CNFailure class]];
}

- (id)get {
    @throw [NSString stringWithFormat:@"Getting failure try: %@", _reason];
}

- (BOOL)isSuccess {
    return NO;
}

- (BOOL)isFailure {
    return YES;
}

- (CNTry*)mapF:(id(^)(id))f {
    return ((CNTry*)(self));
}

- (ODClassType*)type {
    return [CNFailure type];
}

+ (ODClassType*)type {
    return _CNFailure_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"reason=%@", self.reason];
    [description appendString:@">"];
    return description;
}

@end


