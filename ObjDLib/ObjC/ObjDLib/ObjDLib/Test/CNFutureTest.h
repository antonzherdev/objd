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
@class CNClassType;

@class CNFutureTest;

@interface CNFutureTest : TSTestCase
+ (instancetype)futureTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testPromiseOnComplete;
- (void)testMap;
- (void)testFlatMap;
+ (CNClassType*)type;
@end


