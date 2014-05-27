#import "objd.h"

@implementation CNEnum
static CNClassType* _CNEnum_type;
@synthesize ordinal = _ordinal;
@synthesize name = _name;

+ (instancetype)enumWithOrdinal:(NSUInteger)ordinal name:(NSString*)name {
    return [[CNEnum alloc] initWithOrdinal:ordinal name:name];
}

- (instancetype)initWithOrdinal:(NSUInteger)ordinal name:(NSString*)name {
    self = [super init];
    if(self) {
        _ordinal = ordinal;
        _name = name;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNEnum class]) _CNEnum_type = [CNClassType classTypeWithCls:[CNEnum class]];
}

- (NSString*)description {
    return _name;
}

- (NSUInteger)hash {
    return _ordinal;
}

+ (NSArray*)values {
    @throw @"Method values is abstract";
}

- (NSInteger)compareTo:(CNEnum*)to {
    return uintCompareTo(_ordinal, ((CNEnum*)(to)).ordinal);
}

- (CNClassType*)type {
    return [CNEnum type];
}

+ (CNClassType*)type {
    return _CNEnum_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

