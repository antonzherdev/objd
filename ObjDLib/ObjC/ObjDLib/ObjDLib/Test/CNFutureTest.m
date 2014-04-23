#import "objd.h"
#import "CNFutureTest.h"

#import "CNAtomic.h"
#import "CNRange.h"
#import "CNCollection.h"
#import "CNFuture.h"
#import "CNDispatchQueue.h"
#import "CNTry.h"
#import "ODType.h"
@implementation CNFutureTest
static ODClassType* _CNFutureTest_type;

+ (instancetype)futureTest {
    return [[CNFutureTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFutureTest class]) _CNFutureTest_type = [ODClassType classTypeWithCls:[CNFutureTest class]];
}

- (void)testPromiseOnComplete {
    CNAtomicInt* n = [CNAtomicInt atomicInt];
    NSInteger count = 100000;
    {
        id<CNIterator> __inline__2_i = [intTo(1, count) iterator];
        while([__inline__2_i hasNext]) {
            id i = [__inline__2_i next];
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
        id<CNIterator> __inline__3_i = [intTo(1, count) iterator];
        while([__inline__3_i hasNext]) {
            id __inline__3_v = [__inline__3_i next];
            [CNDispatchQueue.aDefault asyncF:^void() {
                CNPromise* p = [CNPromise apply];
                CNFuture* m = [p mapF:^id(id _) {
                    return numi(unumi(_) + 1);
                }];
                [result addAndGetValue:((int)(unumi(__inline__3_v) + 1))];
                [CNDispatchQueue.aDefault asyncF:^void() {
                    [p successValue:__inline__3_v];
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
        id<CNIterator> __inline__3_i = [intTo(1, count) iterator];
        while([__inline__3_i hasNext]) {
            id i = [__inline__3_i next];
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

- (ODClassType*)type {
    return [CNFutureTest type];
}

+ (ODClassType*)type {
    return _CNFutureTest_type;
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


