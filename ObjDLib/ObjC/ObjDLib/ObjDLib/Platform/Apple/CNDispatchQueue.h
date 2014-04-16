#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNDispatchQueue;

@interface CNDispatchQueue : NSObject
@property (nonatomic, readonly) dispatch_queue_t ref;

+ (id)dispatchQueueWithRef:(dispatch_queue_t)ref;
- (id)initWithRef:(dispatch_queue_t)ref;
- (ODClassType*)type;
- (void)asyncF:(void(^)())f;
+ (CNDispatchQueue*)aDefault;
+ (ODClassType*)type;

+ (CNDispatchQueue *)mainThread;
@end


