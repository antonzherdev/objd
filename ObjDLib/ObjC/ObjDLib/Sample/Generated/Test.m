#import "Test.h"

@implementation Foo
static CNClassType* _Foo_type;

+ (instancetype)fooWithBar:(NSInteger)bar {
    return [[Foo alloc] initWithBar:bar];
}

- (instancetype)initWithBar:(NSInteger)bar {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [Foo class]) _Foo_type = [CNClassType classTypeWithCls:[Foo class]];
}

- (NSString*)description {
    return @"Foo";
}

- (CNClassType*)type {
    return [Foo type];
}

+ (CNClassType*)type {
    return _Foo_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

