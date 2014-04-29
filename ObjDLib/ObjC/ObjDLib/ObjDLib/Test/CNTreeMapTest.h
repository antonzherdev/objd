#import "objdcore.h"
#import "TSTestCase.h"
@class CNMTreeMap;
@class CNChain;
@class CNMTreeMapKeySet;
@class CNClassType;

@class CNTreeMapTest;

@interface CNTreeMapTest : TSTestCase
+ (instancetype)treeMapTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testMain;
+ (CNClassType*)type;
@end


