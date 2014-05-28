#import "objd.h"
#import "CNActor.h"
#import "TSTestCase.h"
@class CNFuture;
@class CNChain;
@class CNPromise;

@class CNTestedActor;
@class CNActorTest;

@interface CNTestedActor : CNActor {
@protected
    NSArray* _items;
}
@property (nonatomic) NSArray* items;

+ (instancetype)testedActor;
- (instancetype)init;
- (CNClassType*)type;
- (CNFuture*)addNumber:(NSString*)number;
- (CNFuture*)getItems;
- (CNFuture*)getItemsF;
- (CNFuture*)lockFuture:(CNFuture*)future;
- (CNFuture*)lockVoidFuture:(CNFuture*)future;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNActorTest : TSTestCase
+ (instancetype)actorTest;
- (instancetype)init;
- (CNClassType*)type;
- (void)testTypedActor;
- (void)testTypedActor2;
- (void)testLock;
- (NSString*)description;
+ (CNClassType*)type;
@end


