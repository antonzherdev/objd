#import "objd.h"
@class CNPromise;
@class CNAtomicInt;
@class CNAtomicBool;
@class CNFuture;
@class CNYield;

@class CNFutureEnd;
@class CNFutureVoidEnd;

@interface CNFutureEnd : NSObject {
@protected
    CNPromise* __promise;
    BOOL __stopped;
    CNAtomicInt* __counter;
    volatile BOOL __ended;
    CNAtomicBool* __yielded;
    CNMArray* __array;
}
+ (instancetype)futureEnd;
- (instancetype)init;
- (CNClassType*)type;
- (CNFuture*)future;
- (CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNFutureVoidEnd : NSObject {
@protected
    CNPromise* __promise;
    BOOL __stopped;
    CNAtomicInt* __counter;
    volatile BOOL __ended;
    CNAtomicBool* __yielded;
}
+ (instancetype)futureVoidEnd;
- (instancetype)init;
- (CNClassType*)type;
- (CNFuture*)future;
- (CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


