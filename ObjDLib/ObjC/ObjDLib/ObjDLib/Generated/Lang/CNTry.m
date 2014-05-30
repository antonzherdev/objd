#import "objd.h"
#import "CNTry.h"

#import "CNType.h"
#import "CNString.h"
@implementation CNTry
static CNClassType* _CNTry_type;

+ (instancetype)try {
    return [[CNTry alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTry class]) _CNTry_type = [CNClassType classTypeWithCls:[CNTry class]];
}

- (id)get {
    @throw @"Method get is abstract";
}

- (id)reason {
    @throw @"Method reason is abstract";
}

- (id)value {
    @throw @"Method value is abstract";
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

- (NSString*)description {
    return @"Try";
}

- (CNClassType*)type {
    return [CNTry type];
}

+ (CNClassType*)type {
    return _CNTry_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNSuccess
static CNClassType* _CNSuccess_type;
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
    if(self == [CNSuccess class]) _CNSuccess_type = [CNClassType classTypeWithCls:[CNSuccess class]];
}

- (id)value {
    return _get;
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

- (NSString*)description {
    return [NSString stringWithFormat:@"Success(%@)", _get];
}

- (CNClassType*)type {
    return [CNSuccess type];
}

+ (CNClassType*)type {
    return _CNSuccess_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNFailure
static CNClassType* _CNFailure_type;
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
    if(self == [CNFailure class]) _CNFailure_type = [CNClassType classTypeWithCls:[CNFailure class]];
}

- (id)value {
    return nil;
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

- (NSString*)description {
    return [NSString stringWithFormat:@"Failure(%@)", _reason];
}

- (CNClassType*)type {
    return [CNFailure type];
}

+ (CNClassType*)type {
    return _CNFailure_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

