#import "ODEnum.h"

@implementation ODEnum{
    NSUInteger _ordinal;
    NSString* _name;
}
@synthesize ordinal = _ordinal;
@synthesize name = _name;

+ (id)enumWithOrdinal:(NSUInteger)ordinal name:(NSString*)name {
    return [[ODEnum alloc] initWithOrdinal:ordinal name:name];
}

- (id)initWithOrdinal:(NSUInteger)ordinal name:(NSString*)name {
    self = [super init];
    if(self) {
        _ordinal = ordinal;
        _name = name;
    }
    
    return self;
}

- (NSString*)description {
    return _name;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    return other == self;
}

- (NSUInteger)hash {
    return _ordinal;
}

- (NSInteger)compareTo:(ODEnum*)to {
    return intCompareTo(_ordinal, to.ordinal);
}


@end


