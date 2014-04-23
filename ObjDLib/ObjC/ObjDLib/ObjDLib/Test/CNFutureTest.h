#import "objdcore.h"
#import "TSTestCase.h"
@class CNAtomicInt;
@class CNRange;
@protocol CNIterator;
@class CNPromise;
@class CNDispatchQueue;
@class CNThread;
@class CNTry;
@class CNFuture;
@class ODClassType;

@class CNFutureTest;

@interface CNFutureTest : TSTestCase
+ (instancetype)futureTest;
- (instancetype)init;
- (ODClassType*)type;
- (void)testPromiseOnComplete;
- (void)testMap;
- (void)testFlatMap;
+ (ODClassType*)type;
@end


