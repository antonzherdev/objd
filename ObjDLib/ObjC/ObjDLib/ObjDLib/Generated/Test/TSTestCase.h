#import "objd.h"
#import <XCTest/XCTest.h>

@class TSTestCase;

@interface TSTestCase : XCTestCase
+ (id)testCase;
- (id)init;

- (void)repeatTimes:(int)n f:(void (^)())f;
@end

#define assertEquals(a, b) XCTAssertEqualObjects(a, b, @"");
#define assertTrue(t) XCTAssertTrue(t, @"");
#define assertFalse(t) XCTAssertFalse(t, @"");
#define floatArrayEquals(_a, _b) {\
    id<CNSeq> __a = _a;\
    id<CNSeq> __b = _b;\
    BOOL __ok = YES;\
    if(__a.count != _b.count) __ok = NO;\
    else {\
        id <CNIterator> ai = [__a iterator];\
        id <CNIterator> bi = [__b iterator];\
        while ([ai hasNext] && [bi hasNext]) {\
            NSNumber * an = [ai next];\
            NSNumber * bn = [bi next];\
            if(!eqf([an floatValue], [bn floatValue])) {\
                __ok = NO;\
                break;\
            }\
        }\
    }\
    if(!__ok) {\
        XCTFail(@"%@ != %@", _a, _b);\
    }\
}
#define fail(m) XCTFail(@"%@", m)


