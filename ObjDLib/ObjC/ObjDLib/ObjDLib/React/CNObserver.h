#import "objd.h"
@class CNAtomicObject;
@class CNChain;

@class CNObservable_impl;
@class CNObservableBase_impl;
@class CNObserver;
@class CNSignal;
@protocol CNObservable;
@protocol CNObservableBase;

@protocol CNObservable<NSObject>
- (void)attachObserver:(CNObserver*)observer;
- (void)detachObserver:(CNObserver*)observer;
- (CNObserver*)observeF:(void(^)(id))f;
- (NSString*)description;
@end


@interface CNObservable_impl : NSObject<CNObservable>
+ (instancetype)observable_impl;
- (instancetype)init;
@end


@protocol CNObservableBase<CNObservable>
- (void)attachObserver:(CNObserver*)observer;
- (void)detachObserver:(CNObserver*)observer;
- (void)notifyValue:(id)value;
- (BOOL)hasObservers;
- (NSString*)description;
@end


@interface CNObservableBase_impl : CNObservable_impl<CNObservableBase> {
@protected
    CNAtomicObject* __observers;
}
+ (instancetype)observableBase_impl;
- (instancetype)init;
- (void)attachObserver:(CNObserver*)observer;
- (void)detachObserver:(CNObserver*)observer;
@end


@interface CNObserver : NSObject {
@protected
    id<CNObservable> _observable;
    void(^_f)(id);
}
@property (nonatomic, readonly) id<CNObservable> observable;
@property (nonatomic, readonly) void(^f)(id);

+ (instancetype)observerWithObservable:(id<CNObservable>)observable f:(void(^)(id))f;
- (instancetype)initWithObservable:(id<CNObservable>)observable f:(void(^)(id))f;
- (CNClassType*)type;
- (void)detach;
- (void)dealloc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNSignal : CNObservableBase_impl
+ (instancetype)signal;
- (instancetype)init;
- (CNClassType*)type;
- (void)postData:(id)data;
- (void)post;
- (NSString*)description;
+ (CNClassType*)type;
@end


