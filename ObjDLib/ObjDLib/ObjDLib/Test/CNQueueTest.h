#import "objdcore.h"
#import "TSTestCase.h"
@class CNImQueue;
@class ODClassType;

@class CNQueueTest;

@interface CNQueueTest : TSTestCase
+ (instancetype)queueTest;
- (instancetype)init;
- (ODClassType*)type;
- (void)testDeque;
+ (ODClassType*)type;
@end


