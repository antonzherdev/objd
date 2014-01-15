#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNLog;

@interface CNLog : NSObject
+ (id)log;
- (id)init;
- (ODClassType*)type;
+ (void)applyText:(NSString*)text;
+ (ODClassType*)type;
@end


