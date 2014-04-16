#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNTry;
@class CNSuccess;
@class CNFailure;

@interface CNTry : NSObject
+ (instancetype)try;
- (instancetype)init;
- (ODClassType*)type;
- (id)get;
- (id)reason;
- (BOOL)isSuccess;
- (BOOL)isFailure;
- (CNTry*)mapF:(id(^)(id))f;
+ (ODClassType*)type;
@end


@interface CNSuccess : CNTry {
@protected
    id _get;
}
@property (nonatomic, readonly) id get;

+ (instancetype)successWithGet:(id)get;
- (instancetype)initWithGet:(id)get;
- (ODClassType*)type;
- (BOOL)isSuccess;
- (BOOL)isFailure;
- (id)reason;
- (CNTry*)mapF:(id(^)(id))f;
+ (ODClassType*)type;
@end


@interface CNFailure : CNTry {
@protected
    id _reason;
}
@property (nonatomic, readonly) id reason;

+ (instancetype)failureWithReason:(id)reason;
- (instancetype)initWithReason:(id)reason;
- (ODClassType*)type;
- (id)get;
- (BOOL)isSuccess;
- (BOOL)isFailure;
- (CNTry*)mapF:(id(^)(id))f;
+ (ODClassType*)type;
@end


