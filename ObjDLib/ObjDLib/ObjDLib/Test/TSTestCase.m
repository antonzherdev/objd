#import "TSTestCase.h"

@implementation TSTestCase

+ (id)testCase {
    return [[TSTestCase alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

- (void)assertEqualsA:(id)a b:(id)b {
    XCTAssertEqualObjects(a, b, @"!=");
}

- (void)assertTrueValue:(BOOL)value {
    XCTAssertTrue(value, @"Is not true");
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
//    TSTestCase * o = ((TSTestCase *)other);
    return NO;
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


