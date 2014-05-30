#import "objdcore.h"
#import "TSTestCase.h"
#import "CNObject.h"
@class CNImQueue;
@class CNTuple;
@class CNClassType;
@class CNString;

@class CNQueueTest;

@interface CNQueueTest : TSTestCase
+ (instancetype)queueTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testDeque;
- (NSString*)description;
+ (CNClassType*)type;
@end


