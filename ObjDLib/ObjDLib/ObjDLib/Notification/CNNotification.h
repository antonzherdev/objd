#import "objdcore.h"
#import "ODObject.h"
@class CNNotificationCenter;
@class CNNotificationObserver;
@class ODClassType;

@class CNNotificationHandle;

@interface CNNotificationHandle : NSObject {
@protected
    NSString* _name;
}
@property (nonatomic, readonly) NSString* name;

+ (instancetype)notificationHandleWithName:(NSString*)name;
- (instancetype)initWithName:(NSString*)name;
- (ODClassType*)type;
- (void)postSender:(id)sender;
- (void)postSender:(id)sender data:(id)data;
- (CNNotificationObserver*)observeBy:(void(^)(id, id))by;
- (CNNotificationObserver*)observeSender:(id)sender by:(void(^)(id))by;
+ (ODClassType*)type;
@end


