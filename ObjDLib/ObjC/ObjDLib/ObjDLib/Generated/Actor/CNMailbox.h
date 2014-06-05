#import "objd.h"
#import "CNFuture.h"
@class CNAtomicBool;
@class CNConcurrentQueue;
@class CNDispatchQueue;
@class CNActor;

@class CNMailbox;
@class CNActorMessage_impl;
@class CNActorFuture;
@protocol CNActorMessage;

@interface CNMailbox : NSObject {
@public
    volatile BOOL __stopped;
    CNAtomicBool* __scheduled;
    CNConcurrentQueue* __queue;
    volatile BOOL __locked;
}
+ (instancetype)mailbox;
- (instancetype)init;
- (CNClassType*)type;
- (void)sendMessage:(id<CNActorMessage>)message;
- (void)unlock;
- (void)stop;
- (BOOL)isEmpty;
- (NSString*)description;
+ (CNClassType*)type;
@end


@protocol CNActorMessage<NSObject>
- (CNActor*)receiver;
- (BOOL)prompt;
- (BOOL)process;
- (NSString*)description;
@end


@interface CNActorMessage_impl : NSObject<CNActorMessage>
+ (instancetype)actorMessage_impl;
- (instancetype)init;
@end


@interface CNActorFuture : CNDefaultPromise<CNActorMessage> {
@public
    CNActor* _receiver;
    BOOL _prompt;
    id(^_f)();
    volatile BOOL __completed;
    volatile BOOL __locked;
}
@property (nonatomic, readonly) CNActor* receiver;
@property (nonatomic, readonly) BOOL prompt;
@property (nonatomic, readonly) id(^f)();

+ (instancetype)actorFutureWithReceiver:(CNActor*)receiver prompt:(BOOL)prompt f:(id(^)())f;
- (instancetype)initWithReceiver:(CNActor*)receiver prompt:(BOOL)prompt f:(id(^)())f;
- (CNClassType*)type;
- (BOOL)process;
- (void)lock;
- (void)unlock;
- (BOOL)isLocked;
- (BOOL)completeValue:(CNTry*)value;
- (NSString*)description;
+ (CNClassType*)type;
@end


