#import "objd.h"
#import "TSTestCase.h"
@class CNSignal;
@class CNObserver;

@class CNObserverTest;

@interface CNObserverTest : TSTestCase
+ (instancetype)observerTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testSignal;
- (NSString*)description;
+ (CNClassType*)type;
@end


