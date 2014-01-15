#import "objd.h"
#import <XCTest/XCTest.h>

@class TSTestCase;

@interface TSTestCase : XCTestCase
+ (id)testCase;
- (id)init;
- (void)assertEqualsA:(id)a b:(id)b;
- (void)assertTrueValue:(BOOL)value;
@end


