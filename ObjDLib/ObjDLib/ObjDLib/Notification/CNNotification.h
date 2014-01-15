#import "objdcore.h"
#import "ODObject.h"
@class CNNotificationCenter;
@class CNNotificationObserver;
@class ODClassType;

@class CNNotificationHandle;

@interface CNNotificationHandle : NSObject
@property (nonatomic, readonly) NSString* name;

+ (id)notificationHandleWithName:(NSString*)name;
- (id)initWithName:(NSString*)name;
- (ODClassType*)type;
- (void)postSender:(id)sender;
- (void)postSender:(id)sender data:(id)data;
- (CNNotificationObserver*)observeBy:(void(^)(id, id))by;
- (CNNotificationObserver*)observeSender:(id)sender by:(void(^)(id))by;
+ (ODClassType*)type;
@end


