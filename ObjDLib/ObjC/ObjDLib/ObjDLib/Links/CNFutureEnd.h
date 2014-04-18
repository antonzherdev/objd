#import "objdcore.h"
#import "ODObject.h"
@class CNPromise;
@class CNAtomicInt;
@class CNAtomicBool;
@class CNMArray;
@class CNFuture;
@class CNYield;
@class CNTry;
@class ODClassType;

@class CNFutureEnd;
@class CNFutureVoidEnd;

@interface CNFutureEnd : NSObject {
@protected
    CNPromise* __promise;
    BOOL __stopped;
    CNAtomicInt* __counter;
    BOOL __ended;
    CNAtomicBool* __yielded;
    CNMArray* __array;
}
+ (instancetype)futureEnd;
- (instancetype)init;
- (ODClassType*)type;
- (CNFuture*)future;
- (CNYield*)yield;
+ (ODClassType*)type;
@end


@interface CNFutureVoidEnd : NSObject {
@protected
    CNPromise* __promise;
    BOOL __stopped;
    CNAtomicInt* __counter;
    BOOL __ended;
    CNAtomicBool* __yielded;
}
+ (instancetype)futureVoidEnd;
- (instancetype)init;
- (ODClassType*)type;
- (CNFuture*)future;
- (CNYield*)yield;
+ (ODClassType*)type;
@end


