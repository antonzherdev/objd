#import "objd.h"
#import "CNFutureEnd.h"

#import "CNFuture.h"
#import "CNAtomic.h"
#import "CNPlat.h"
#import "CNYield.h"
#import "CNTry.h"
#import "CNType.h"
@implementation CNFutureEnd
static CNClassType* _CNFutureEnd_type;

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
    if(self == [CNFutureEnd class]) _CNFutureEnd_type = [CNClassType classTypeWithCls:[CNFutureEnd class]];
}

- (CNFuture*)future {
    return __promise;
}

- (CNYield*)yield {
    __block NSInteger _i = 0;
    __block volatile NSInteger _set = -1;
    return [CNYield makeBegin:^CNGoR(NSUInteger size) {
        __array = [CNMArray applyCapacity:size];
        return CNGo_Continue;
    } yield:^CNGoR(CNFuture* fut) {
        if(!(__stopped)) {
            [__counter incrementAndGet];
            [((CNMArray*)(nonnil(__array))) appendItem:nil];
            NSInteger i = _i;
            _i++;
            [((CNFuture*)(fut)) onCompleteF:^void(CNTry* tr) {
                if(!(__stopped)) {
                    if([tr isFailure]) {
                        __stopped = YES;
                        [__promise failureReason:tr];
                    } else {
                        if(!(__stopped)) {
                            [((CNMArray*)(nonnil(__array))) setIndex:((NSUInteger)(i)) item:[tr get]];
                            _set = i;
                            int r = [__counter decrementAndGet];
                            if(__ended && r == 0) {
                                if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:((CNMArray*)(nonnil(__array)))];
                            }
                        }
                    }
                }
            }];
        }
        if(__stopped) return CNGo_Break;
        else return CNGo_Continue;
    } end:^CNGoR(CNGoR res) {
        __ended = YES;
        if([__counter intValue] == 0) {
            if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:((CNMArray*)(nonnil(__array)))];
        }
        return res;
    }];
}

- (NSString*)description {
    return @"FutureEnd";
}

- (CNClassType*)type {
    return [CNFutureEnd type];
}

+ (CNClassType*)type {
    return _CNFutureEnd_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNFutureVoidEnd
static CNClassType* _CNFutureVoidEnd_type;

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
    if(self == [CNFutureVoidEnd class]) _CNFutureVoidEnd_type = [CNClassType classTypeWithCls:[CNFutureVoidEnd class]];
}

- (CNFuture*)future {
    return __promise;
}

- (CNYield*)yield {
    return [CNYield makeBegin:^CNGoR(NSUInteger size) {
        return CNGo_Continue;
    } yield:^CNGoR(CNFuture* fut) {
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
                            if(__ended && r == 0) {
                                if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:nil];
                            }
                        }
                    }
                }
            }];
        }
        if(__stopped) return CNGo_Break;
        else return CNGo_Continue;
    } end:^CNGoR(CNGoR res) {
        CNGoR ret = res;
        __ended = YES;
        if([__counter intValue] == 0) {
            if(!([__yielded getAndSetNewValue:YES])) [__promise successValue:nil];
        }
        return ret;
    }];
}

- (NSString*)description {
    return @"FutureVoidEnd";
}

- (CNClassType*)type {
    return [CNFutureVoidEnd type];
}

+ (CNClassType*)type {
    return _CNFutureVoidEnd_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

