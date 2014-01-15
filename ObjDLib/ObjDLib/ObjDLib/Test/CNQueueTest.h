#import "objdcore.h"
#import "TSTestCase.h"
@class CNQueue;
@class ODClassType;

@class CNQueueTest;

@interface CNQueueTest : TSTestCase
+ (id)queueTest;
- (id)init;
- (ODClassType*)type;
- (void)testDeque;
+ (ODClassType*)type;
@end


