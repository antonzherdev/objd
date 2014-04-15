#import "objdcore.h"
#import "ODObject.h"
@class CNDispatchQueue;
@class CNAtomicInt;
@class CNTry;
@class CNSuccess;
@protocol CNTraversable;
@class CNTuple;
@class ODClassType;
@class CNAtomicObject;
@class CNFailure;

@class CNFuture;
@class CNPromise;
@class CNDefaultPromise;
@class CNKeptPromise;

@interface CNFuture : NSObject
+ (instancetype)future;
- (instancetype)init;
- (ODClassType*)type;
+ (CNFuture*)applyF:(id(^)())f;
+ (CNFuture*)joinA:(CNFuture*)a b:(CNFuture*)b;
+ (CNFuture*)joinA:(CNFuture*)a b:(CNFuture*)b c:(CNFuture*)c;
+ (CNFuture*)joinA:(CNFuture*)a b:(CNFuture*)b c:(CNFuture*)c d:(CNFuture*)d;
+ (CNFuture*)joinA:(CNFuture*)a b:(CNFuture*)b c:(CNFuture*)c d:(CNFuture*)d e:(CNFuture*)e;
+ (CNFuture*)mapA:(CNFuture*)a b:(CNFuture*)b f:(id(^)(id, id))f;
+ (CNFuture*)mapA:(CNFuture*)a b:(CNFuture*)b c:(CNFuture*)c f:(id(^)(id, id, id))f;
+ (CNFuture*)mapA:(CNFuture*)a b:(CNFuture*)b c:(CNFuture*)c d:(CNFuture*)d f:(id(^)(id, id, id, id))f;
+ (CNFuture*)mapA:(CNFuture*)a b:(CNFuture*)b c:(CNFuture*)c d:(CNFuture*)d e:(CNFuture*)e f:(id(^)(id, id, id, id, id))f;
+ (CNFuture*)successfulResult:(id)result;
- (CNTry*)result;
- (BOOL)isCompleted;
- (BOOL)isSucceeded;
- (BOOL)isFailed;
- (void)onCompleteF:(void(^)(CNTry*))f;
- (CNFuture*)mapF:(id(^)(id))f;
- (CNFuture*)forF:(void(^)(id))f;
- (CNFuture*)flatMapF:(CNFuture*(^)(id))f;
- (CNTry*)waitResultPeriod:(CGFloat)period;
- (CNTry*)waitResult;
- (id)getResultAwait:(CGFloat)await;
- (CNFuture*)joinAnother:(CNFuture*)another;
+ (ODClassType*)type;
@end


@interface CNPromise : CNFuture
+ (instancetype)promise;
- (instancetype)init;
- (ODClassType*)type;
+ (CNPromise*)apply;
- (BOOL)completeValue:(CNTry*)value;
- (BOOL)successValue:(id)value;
- (BOOL)failureReason:(id)reason;
+ (ODClassType*)type;
@end


@interface CNDefaultPromise : CNPromise {
@protected
    CNAtomicObject* __state;
}
+ (instancetype)defaultPromise;
- (instancetype)init;
- (ODClassType*)type;
- (CNTry*)result;
- (BOOL)completeValue:(CNTry*)value;
- (BOOL)successValue:(id)value;
- (BOOL)failureReason:(id)reason;
- (void)onCompleteF:(void(^)(CNTry*))f;
+ (ODClassType*)type;
@end


@interface CNKeptPromise : CNPromise {
@protected
    CNTry* _value;
}
@property (nonatomic, readonly) CNTry* value;

+ (instancetype)keptPromiseWithValue:(CNTry*)value;
- (instancetype)initWithValue:(CNTry*)value;
- (ODClassType*)type;
- (CNTry*)result;
- (void)onCompleteF:(void(^)(CNTry*))f;
- (CNTry*)waitResultPeriod:(CGFloat)period;
- (CNTry*)waitResult;
- (BOOL)completeValue:(CNTry*)value;
- (BOOL)successValue:(id)value;
- (BOOL)failureReason:(id)reason;
+ (ODClassType*)type;
@end


