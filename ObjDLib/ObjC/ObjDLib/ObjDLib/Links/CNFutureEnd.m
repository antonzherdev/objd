#import "objd.h"
#import "CNFutureEnd.h"

#import "CNFuture.h"
#import "CNAtomic.h"
#import "CNYield.h"
#import "ObjC.h"
#import "CNTry.h"
#import "CNTypes.h"
#import "ODType.h"
@implementation CNFutureEnd
static ODClassType* _CNFutureEnd_type;

+ (instancetype)futureEnd {
    return [[CNFutureEnd alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) {
        __promise = [CNPromise apply];
        __stopped = NO;
        __counter = [CNAtomicInt atomicInt];
        __ended = NO;
        __yielded = [CNAtomicBool atomicBool];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFutureEnd class]) _CNFutureEnd_type = [ODClassType classTypeWithCls:[CNFutureEnd class]];
}

- (CNFuture*)future {
    return __promise;
}

- (CNYield*)yield {
    __block NSInteger _i = 0;
    return [CNYield applyBegin:^NSInteger(NSUInteger size) {
        __array = [NSMutableArray applyCapacity:size];
        return 0;
    } yield:^NSInteger(CNFuture* fut) {
        if(!(__stopped)) {
            [__counter incrementAndGet];
            [((NSMutableArray*)(nonnil(__array))) appendItem:nil];
            NSInteger i = _i;
            _i++;
            [((CNFuture*)(fut)) onCompleteF:^void(CNTry* tr) {
                if(!(__stopped)) {
                    if([tr isFailure]) {
                        __stopped = YES;
                        [__promise failureReason:tr];
                    } else {
                        if(!(__stopped)) {
                            [((NSMutableArray*)(nonnil(__array))) setIndex:((NSUInteger)(i)) item:[tr get]];
                            memoryBarrier();
                            int r = [__counter decrementAndGet];
                            memoryBarrier();
                            if(__ended && r == 0) {
                                memoryBarrier();
                                if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:((NSMutableArray*)(nonnil(__array)))];
                            }
                        }
                    }
                }
            }];
        }
        if(__stopped) return 1;
        else return 0;
    } end:^NSInteger(NSInteger res) {
        __ended = YES;
        memoryBarrier();
        if([__counter intValue] == 0) {
            memoryBarrier();
            if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:((NSMutableArray*)(nonnil(__array)))];
        }
        return res;
    }];
}

- (ODClassType*)type {
    return [CNFutureEnd type];
}

+ (ODClassType*)type {
    return _CNFutureEnd_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNFutureVoidEnd
static ODClassType* _CNFutureVoidEnd_type;

+ (instancetype)futureVoidEnd {
    return [[CNFutureVoidEnd alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) {
        __promise = [CNPromise apply];
        __stopped = NO;
        __counter = [CNAtomicInt atomicInt];
        __ended = NO;
        __yielded = [CNAtomicBool atomicBool];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFutureVoidEnd class]) _CNFutureVoidEnd_type = [ODClassType classTypeWithCls:[CNFutureVoidEnd class]];
}

- (CNFuture*)future {
    return __promise;
}

- (CNYield*)yield {
    return [CNYield applyBegin:^NSInteger(NSUInteger size) {
        return 0;
    } yield:^NSInteger(CNFuture* fut) {
        if(!(__stopped)) {
            [__counter incrementAndGet];
            [((CNFuture*)(fut)) onCompleteF:^void(CNTry* tr) {
                if(!(__stopped)) {
                    if([tr isFailure]) {
                        __stopped = YES;
                        [__promise failureReason:tr];
                    } else {
                        if(!(__stopped)) {
                            int r = [__counter decrementAndGet];
                            memoryBarrier();
                            if(__ended && r == 0) {
                                memoryBarrier();
                                if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:nil];
                            }
                        }
                    }
                }
            }];
        }
        if(__stopped) return 1;
        else return 0;
    } end:^NSInteger(NSInteger res) {
        NSInteger ret = res;
        __ended = YES;
        memoryBarrier();
        if([__counter intValue] == 0) {
            memoryBarrier();
            if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:nil];
        }
        return ret;
    }];
}

- (ODClassType*)type {
    return [CNFutureVoidEnd type];
}

+ (ODClassType*)type {
    return _CNFutureVoidEnd_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


