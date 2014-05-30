#import "objd.h"
@class CNMailbox;
@class CNFuture;
@class CNActorFuture;
@class CNPromise;

@class CNActor;



@interface CNActor : NSObject {
@protected
    CNMailbox* _mailbox;
}
@property (nonatomic, readonly) CNMailbox* mailbox;

+ (instancetype)actor;
- (instancetype)init;
- (CNClassType*)type;
- (CNFuture*)futureF:(id(^)())f;
- (CNFuture*)promptF:(id(^)())f;
- (CNFuture*)futureJoinF:(CNFuture*(^)())f;
- (CNFuture*)promptJoinF:(CNFuture*(^)())f;
- (CNFuture*)onSuccessFuture:(CNFuture*)future f:(id(^)(id))f;
- (CNFuture*)lockAndOnSuccessFuture:(CNFuture*)future f:(id(^)(id))f;
- (CNFuture*)dummy;
- (NSString*)description;
+ (CNClassType*)type;
@end


