#import "objd.h"
#import "TSTestCase.h"
@class CNAtomicInt;
@class CNPromise;
@class CNDispatchQueue;
@class CNThread;
@class CNFuture;

@class CNFutureTest;

@interface CNFutureTest : TSTestCase
+ (instancetype)futureTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testPromiseOnComplete;
- (void)testMap;
- (void)testFlatMap;
- (NSString*)description;
+ (CNClassType*)type;
@end


