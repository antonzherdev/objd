#import "objd.h"
#import "CNFutureTest.h"

#import "CNAtomic.h"
#import "CNFuture.h"
#import "CNDispatchQueue.h"
#import "CNRange.h"
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
    [intTo(1, count) forEach:^void(id i) {
        CNPromise* p = [CNPromise apply];
        [CNDispatchQueue.aDefault asyncF:^void() {
            [p successValue:i];
        }];
        [p onCompleteF:^void(CNTry* _) {
            [n incrementAndGet];
        }];
    }];
    [CNThread sleepPeriod:1.0];
    assertEquals(numi4([n intValue]), numi4(((int)(count))));
}

- (void)testMap {
    CNAtomicInt* n = [CNAtomicInt atomicInt];
    NSInteger count = 100;
    CNAtomicInt* result = [CNAtomicInt atomicInt];
    [intTo(1, count) parForEach:^void(id i) {
        CNPromise* p = [CNPromise apply];
        CNFuture* m = [p mapF:^id(id _) {
            return numi(unumi(_) + 1);
        }];
        [result addAndGetValue:((int)(unumi(i) + 1))];
        [CNDispatchQueue.aDefault asyncF:^void() {
            [p successValue:i];
        }];
        [m onCompleteF:^void(CNTry* _) {
            [n addAndGetValue:((int)(unumi([_ get])))];
        }];
    }];
    [CNThread sleepPeriod:3.0];
    assertEquals(n, result);
}

- (void)testFlatMap {
    CNAtomicInt* n = [CNAtomicInt atomicInt];
    NSInteger count = 100;
    __block NSInteger result = 0;
    [intTo(1, count) forEach:^void(id i) {
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
    }];
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


