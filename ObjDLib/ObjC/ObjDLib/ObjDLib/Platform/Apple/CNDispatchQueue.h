#import "objdcore.h"
#import "CNObject.h"
@class CNClassType;

@class CNDispatchQueue;

@interface CNDispatchQueue : NSObject
@property (nonatomic, readonly) dispatch_queue_t ref;

+ (id)dispatchQueueWithRef:(dispatch_queue_t)ref;
- (id)initWithRef:(dispatch_queue_t)ref;
- (CNClassType *)type;
- (void)asyncF:(void(^)())f;
+ (CNDispatchQueue*)aDefault;
+ (CNClassType *)type;

+ (CNDispatchQueue *)mainThread;
@end


