#import "CNMailbox.h"

#import "CNAtomic.h"
#import "CNConcurrentQueue.h"
#import "CNDispatchQueue.h"
#import "CNActor.h"
@implementation CNMailbox
static CNClassType* _CNMailbox_type;

+ (instancetype)mailbox {
    return [[CNMailbox alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) {
        __stopped = NO;
        __scheduled = [CNAtomicBool atomicBool];
        __queue = [CNConcurrentQueue concurrentQueue];
        __locked = NO;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMailbox class]) _CNMailbox_type = [CNClassType classTypeWithCls:[CNMailbox class]];
}

- (void)sendMessage:(id<CNActorMessage>)message {
    if(__stopped) return ;
    if([message prompt]) {
        if(!([__scheduled getAndSetNewValue:YES])) {
            if([__queue isEmpty]) {
                [message process];
                if([__queue isEmpty]) {
                    [__scheduled setNewValue:NO];
                    if(!([__queue isEmpty])) [self trySchedule];
                } else {
                    [self schedule];
                }
            } else {
                [__queue enqueueItem:message];
                [self schedule];
            }
        } else {
            [__queue enqueueItem:message];
            [self trySchedule];
        }
    } else {
        [__queue enqueueItem:message];
        [self trySchedule];
    }
}

- (void)trySchedule {
    if(!([__scheduled getAndSetNewValue:YES])) [self schedule];
}

- (void)schedule {
    if(!(__stopped)) [CNDispatchQueue.aDefault asyncF:^void() {
        autoreleasePoolStart();
        [self processQueue];
        autoreleasePoolEnd();
    }];
}

- (void)processQueue {
    NSInteger left = 5;
    __locked = NO;
    while(left > 0) {
        id<CNActorMessage> msg = [__queue dequeueWhen:^BOOL(id<CNActorMessage> message) {
            if([((id<CNActorMessage>)(message)) process]) {
                return YES;
            } else {
                __locked = YES;
                return NO;
            }
        }];
        if(msg == nil) break;
        left--;
    }
    if(__locked) {
    } else {
        if([__queue isEmpty]) {
            [__scheduled setNewValue:NO];
            if(!([__queue isEmpty])) [self trySchedule];
        } else {
            [self schedule];
        }
    }
}

- (void)unlock {
    if(__locked) {
        __locked = NO;
        [self schedule];
    }
}

- (void)stop {
    __stopped = YES;
    [__queue clear];
}

- (BOOL)isEmpty {
    return [__queue isEmpty];
}

- (NSString*)description {
    return @"Mailbox";
}

- (CNClassType*)type {
    return [CNMailbox type];
}

+ (CNClassType*)type {
    return _CNMailbox_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNActorMessage_impl

+ (instancetype)actorMessage_impl {
    return [[CNActorMessage_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (CNActor*)receiver {
    @throw @"Method receiver is abstract";
}

- (BOOL)prompt {
    @throw @"Method prompt is abstract";
}

- (BOOL)process {
    @throw @"Method process is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNActorFuture
static CNClassType* _CNActorFuture_type;
@synthesize receiver = _receiver;
@synthesize prompt = _prompt;
@synthesize f = _f;

+ (instancetype)actorFutureWithReceiver:(CNActor*)receiver prompt:(BOOL)prompt f:(id(^)())f {
    return [[CNActorFuture alloc] initWithReceiver:receiver prompt:prompt f:f];
}

- (instancetype)initWithReceiver:(CNActor*)receiver prompt:(BOOL)prompt f:(id(^)())f {
    self = [super init];
    if(self) {
        _receiver = receiver;
        _prompt = prompt;
        _f = [f copy];
        __completed = NO;
        __locked = NO;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNActorFuture class]) _CNActorFuture_type = [CNClassType classTypeWithCls:[CNActorFuture class]];
}

- (BOOL)process {
    if(__completed) {
        return YES;
    } else {
        if(__locked) return NO;
        else return [self successValue:_f()];
    }
}

- (void)lock {
    __locked = YES;
}

- (void)unlock {
    if(__locked) {
        __locked = NO;
        [_receiver.mailbox unlock];
    }
}

- (BOOL)isLocked {
    return __locked;
}

- (BOOL)completeValue:(CNTry*)value {
    BOOL ret = [super completeValue:value];
    if(ret) {
        __completed = YES;
        __locked = NO;
    }
    return ret;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"ActorFuture(%@, %d)", _receiver, _prompt];
}

- (CNClassType*)type {
    return [CNActorFuture type];
}

+ (CNClassType*)type {
    return _CNActorFuture_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

