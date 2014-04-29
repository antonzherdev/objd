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
- (void)testFlat;
- (void)testZip;
- (void)testZip3;
- (void)testZipFor;
+ (CNClassType*)type;
@end


