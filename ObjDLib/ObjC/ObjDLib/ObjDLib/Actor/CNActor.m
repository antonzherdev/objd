#import "CNActor.h"

#import "CNMailbox.h"
#import "CNFuture.h"
@implementation CNActor
static CNClassType* _CNActor_type;
@synthesize mailbox = _mailbox;

+ (instancetype)actor {
    return [[CNActor alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _mailbox = [CNMailbox mailbox];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNActor class]) _CNActor_type = [CNClassType classTypeWithCls:[CNActor class]];
}

- (CNFuture*)futureF:(id(^)())f {
    CNActorFuture* fut = [CNActorFuture actorFutureWithReceiver:self prompt:NO f:f];
    [_mailbox sendMessage:fut];
    return fut;
}

- (CNFuture*)promptF:(id(^)())f {
    CNActorFuture* fut = [CNActorFuture actorFutureWithReceiver:self prompt:YES f:f];
    [_mailbox sendMessage:fut];
    return fut;
}

- (CNFuture*)futureJoinF:(CNFuture*(^)())f {
    CNPromise* ret = [CNPromise apply];
    CNActorFuture* fut = [CNActorFuture actorFutureWithReceiver:self prompt:NO f:^id() {
        {
            CNFuture* nf = f();
            [nf onCompleteF:^void(CNTry* _) {
                [ret completeValue:_];
            }];
        }
        return nil;
    }];
    [_mailbox sendMessage:fut];
    return ret;
}

- (CNFuture*)promptJoinF:(CNFuture*(^)())f {
    CNPromise* ret = [CNPromise apply];
    CNActorFuture* fut = [CNActorFuture actorFutureWithReceiver:self prompt:YES f:^id() {
        {
            CNFuture* nf = f();
            [nf onCompleteF:^void(CNTry* _) {
                [ret completeValue:_];
            }];
        }
        return nil;
    }];
    [_mailbox sendMessage:fut];
    return ret;
}

- (CNFuture*)onSuccessFuture:(CNFuture*)future f:(id(^)(id))f {
    __block id res;
    CNActorFuture* fut = [CNActorFuture actorFutureWithReceiver:self prompt:NO f:^id() {
        return f(res);
    }];
    [future onCompleteF:^void(CNTry* tr) {
        if([tr isFailure]) {
            [fut completeValue:tr];
        } else {
            res = [tr get];
            [_mailbox sendMessage:fut];
        }
    }];
    return fut;
}

- (CNFuture*)lockAndOnSuccessFuture:(CNFuture*)future f:(id(^)(id))f {
    __block id res;
    CNActorFuture* fut = [CNActorFuture actorFutureWithReceiver:self prompt:NO f:^id() {
        return f(res);
    }];
    [fut lock];
    [future onCompleteF:^void(CNTry* tr) {
        if([tr isFailure]) {
            [fut completeValue:tr];
        } else {
            res = [tr get];
            [fut unlock];
        }
    }];
    [_mailbox sendMessage:fut];
    return fut;
}

- (CNFuture*)dummy {
    return [self futureF:^id() {
        return nil;
    }];
}

- (NSString*)description {
    return @"Actor";
}

- (CNClassType*)type {
    return [CNActor type];
}

+ (CNClassType*)type {
    return _CNActor_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

