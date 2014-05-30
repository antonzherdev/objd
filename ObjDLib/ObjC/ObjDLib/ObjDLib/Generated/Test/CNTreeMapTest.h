#import "objdcore.h"
#import "TSTestCase.h"
@class CNMTreeMap;
@class CNString;
@class CNImArray;
@class CNChain;
@class CNMTreeMapKeySet;
@class CNClassType;

@class CNTreeMapTest;

@interface CNTreeMapTest : TSTestCase
+ (instancetype)treeMapTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testMain;
- (NSString*)description;
+ (CNClassType*)type;
@end


