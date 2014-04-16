#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNNotificationCenter;
@class CNNotificationObserver;

@interface CNNotificationCenter : NSObject
+ (id)notificationCenter;
- (id)init;
- (ODClassType*)type;
- (CNNotificationObserver*)addObserverName:(NSString*)name sender:(id)sender block:(void(^)(id, id))block;
- (void)postName:(NSString*)name sender:(id)sender data:(id)data;
+ (CNNotificationCenter*)instance;
+ (ODClassType*)type;
@end


@interface CNNotificationObserver : NSObject
- (instancetype)initWithObserverHandle:(id)observerHandle;

+ (instancetype)observerWithObserverHandle:(id)observerHandle;

- (ODClassType*)type;
- (void)detach;
+ (ODClassType*)type;
@end


