#import "objdcore.h"
#import "TSTestCase.h"
@class CNMTreeMap;
@class CNChain;
@class CNMTreeMapKeySet;
@class ODClassType;

@class CNTreeMapTest;

@interface CNTreeMapTest : TSTestCase
+ (instancetype)treeMapTest;
- (instancetype)init;
- (ODClassType*)type;
- (void)testMain;
+ (ODClassType*)type;
@end


