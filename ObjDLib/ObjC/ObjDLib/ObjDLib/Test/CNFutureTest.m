#import "objd.h"
#import "CNFutureTest.h"

#import "CNAtomic.h"
#import "CNRange.h"
#import "CNCollection.h"
#import "CNFuture.h"
#import "CNDispatchQueue.h"
#import "CNTry.h"
#import "CNType.h"
@implementation CNFutureTest
static CNClassType* _CNFutureTest_type;

+ (instancetype)futureTest {
    return [[CNFutureTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFutureTest class]) _CNFutureTest_type = [CNClassType classTypeWithCls:[CNFutureTest class]];
}

- (void)testPromiseOnComplete {
    CNAtomicInt* n = [CNAtomicInt atomicInt];
    NSInteger count = 100000;
    {
        id<CNIterator> __il__2i = [intTo(1, count) iterator];
        while([__il__2i hasNext]) {
            id i = [__il__2i next];
            {
                CNPromise* p = [CNPromise apply];
                [CNDispatchQueue.aDefault asyncF:^void() {
                    [p successValue:i];
                }];
                [p onCompleteF:^void(CNTry* _) {
                    [n incrementAndGet];
                }];
            }
        }
    }
    [CNThread sleepPeriod:1.0];
    assertEquals(numi4([n intValue]), numi4(((int)(count))));
}

- (void)testMap {
    CNAtomicInt* n = [CNAtomicInt atomicInt];
    NSInteger count = 100;
    CNAtomicInt* result = [CNAtomicInt atomicInt];
    {
        id<CNIterator> __il__3i = [intTo(1, count) iterator];
        while([__il__3i hasNext]) {
            id __il__3v = [__il__3i next];
            [CNDispatchQueue.aDefault asyncF:^void() {
                CNPromise* p = [CNPromise apply];
                CNFuture* m = [p mapF:^id(id _) {
                    return numi(unumi(_) + 1);
                }];
                [result addAndGetValue:((int)(unumi(__il__3v) + 1))];
                [CNDispatchQueue.aDefault asyncF:^void() {
                    [p successValue:__il__3v];
                }];
                [m onCompleteF:^void(CNTry* _) {
                    [n addAndGetValue:((int)(unumi([_ get])))];
                }];
            }];
        }
    }
    [CNThread sleepPeriod:3.0];
    assertEquals(n, result);
}

- (void)testFlatMap {
    CNAtomicInt* n = [CNAtomicInt atomicInt];
    NSInteger count = 100;
    NSInteger result = 0;
    {
        id<CNIterator> __il__3i = [intTo(1, count) iterator];
        while([__il__3i hasNext]) {
            id i = [__il__3i next];
            {
                CNPromise* p = [CNPromise apply];
                CNFuture* m = [p flatMapF:^CNFuture*(id _) {
                    return [CNFuture applyF:^id() {
                        return numi(unumi(_) + 1);
                    }];
                }];
                result += unumi(i) + 1;
                [CNDispatchQueue.aDefault asyncF:^void() {
                    [p successValue:i];
                }];
                [m onCompleteF:^void(CNTry* _) {
                    [n addAndGetValue:((int)(unumi([_ get])))];
                }];
            }
        }
    }
    [CNThread sleepPeriod:3.0];
    assertEquals(numi4([n intValue]), numi4(((int)(result))));
}

- (NSString*)description {
    return @"FutureTest";
}

- (CNClassType*)type {
    return [CNFutureTest type];
}

+ (CNClassType*)type {
    return _CNFutureTest_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

