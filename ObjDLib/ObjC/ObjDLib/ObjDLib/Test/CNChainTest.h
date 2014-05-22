#import "objdcore.h"
#import "TSTestCase.h"
@class CNChain;
@class CNRange;
@class CNPromise;
@class CNDispatchQueue;
@class CNFuture;
@class CNAtomicInt;
@class CNTry;
@class CNClassType;

@class CNChainTest;

@interface CNChainTest : TSTestCase
+ (instancetype)chainTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testAnd;
- (void)testOr;
- (void)testFuture;
- (void)testVoidFuture;
- (void)testMap;
- (void)testMapOpt;
- (void)testFlatMap;
- (void)testFlat;
- (void)testZip;
- (void)testZip3;
- (void)testZipFor;
- (void)testAppend;
- (void)testPreppend;
- (void)testMul;
- (void)testTop;
- (void)testSort;
- (void)testReverse;
- (void)testGroupBy;
- (void)testDistinct;
- (void)testCombinations;
- (void)testUncombinations;
- (void)testNeighbours;
- (void)testExclude;
- (void)testIntersect;
- (void)testFold;
- (void)testCount;
- (void)testHead;
- (void)testLast;
- (void)testRandom;
- (void)testGap;
- (void)testMin;
- (void)testMax;
- (void)testToString;
+ (CNClassType*)type;
@end


