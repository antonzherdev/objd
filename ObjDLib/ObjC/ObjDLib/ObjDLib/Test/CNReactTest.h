#import "objd.h"
#import "TSTestCase.h"
@class CNVar;
@class CNReact;
@class CNReactFlag;

@class CNReactTest;

@interface CNReactTest : TSTestCase
+ (instancetype)reactTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testMap;
- (void)testReactFlag;
- (NSString*)description;
+ (CNClassType*)type;
@end


