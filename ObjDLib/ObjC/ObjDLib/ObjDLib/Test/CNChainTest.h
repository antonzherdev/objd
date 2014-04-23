#import "objdcore.h"
#import "TSTestCase.h"
@class CNChain;
@class CNRange;
@class CNPromise;
@protocol CNIterator;
@class CNDispatchQueue;
@class CNFuture;
@class CNAtomicInt;
@class CNTry;
@class ODClassType;

@class CNChainTest;

@interface CNChainTest : TSTestCase
+ (instancetype)chainTest;
- (instancetype)init;
- (ODClassType*)type;
- (void)testAnd;
- (void)testOr;
- (void)testFuture;
- (void)testVoidFuture;
- (void)testFlat;
- (void)testZip;
- (void)testZip3;
- (void)testZipFor;
+ (ODClassType*)type;
@end


