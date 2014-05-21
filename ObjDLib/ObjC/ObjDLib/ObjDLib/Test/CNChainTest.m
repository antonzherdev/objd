#import "objd.h"
#import "CNChainTest.h"

#import "CNChain.h"
#import "CNRange.h"
#import "CNFuture.h"
#import "CNDispatchQueue.h"
#import "CNAtomic.h"
#import "CNTry.h"
#import "CNType.h"
@implementation CNChainTest
static CNClassType* _CNChainTest_type;

+ (instancetype)chainTest {
    return [[CNChainTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNChainTest class]) _CNChainTest_type = [CNClassType classTypeWithCls:[CNChainTest class]];
}

- (void)testAnd {
    assertTrue((!([[(@[@YES, @NO, @YES]) chain] and])));
    assertTrue((!([[(@[@NO, @NO, @NO]) chain] and])));
    assertTrue(([[(@[@YES, @YES, @YES]) chain] and]));
    assertTrue([[(@[]) chain] and]);
}

- (void)testOr {
    assertTrue(([[(@[@NO, @NO, @YES]) chain] or]));
    assertTrue((!([[(@[@NO, @NO, @NO]) chain] or])));
    assertTrue(([[(@[@YES, @YES, @YES]) chain] or]));
    assertTrue(!([[(@[]) chain] or]));
}

- (void)testFuture {
    [self repeatTimes:1000 f:^void() {
        NSArray* arr = [[[intTo(0, 1000) chain] map:^CNTuple*(id i) {
            return tuple(i, [CNPromise apply]);
        }] toArray];
        for(CNTuple* t in arr) {
            [CNDispatchQueue.aDefault asyncF:^void() {
                [((CNPromise*)(((CNTuple*)(t)).b)) successValue:numi(unumi(((CNTuple*)(t)).a) * unumi(((CNTuple*)(t)).a))];
            }];
        }
        CNFuture* fut = [[[arr chain] map:^CNPromise*(CNTuple* _) {
            return ((CNTuple*)(_)).b;
        }] futureF:^NSArray*(CNChain* chain) {
            return [chain toArray];
        }];
        NSArray* set = [[[[arr chain] map:^id(CNTuple* _) {
            return ((CNTuple*)(_)).a;
        }] map:^id(id _) {
            return numi(unumi(_) * unumi(_));
        }] toArray];
        assertEquals(set, [fut getResultAwait:5.0]);
    }];
}

- (void)testVoidFuture {
    NSArray* arr = [[[intTo(0, 1000) chain] map:^CNPromise*(id i) {
        return [CNPromise apply];
    }] toArray];
    CNFuture* fut = [[arr chain] voidFuture];
    CNAtomicInt* count = [CNAtomicInt atomicInt];
    for(CNPromise* p in arr) {
        [CNDispatchQueue.aDefault asyncF:^void() {
            [count incrementAndGet];
            [((CNPromise*)(p)) successValue:nil];
        }];
    }
    assertTrue([fut waitResultPeriod:5.0] != nil);
    assertEquals(numi4([count intValue]), numi4(((int)([arr count]))));
}

- (void)testMap {
    assertEquals(((@[@4, @0, @2])), ([[[(@[@2, @0, @1]) chain] map:^id(id x) {
        return numi(2 * unumi(x));
    }] toArray]));
}

- (void)testMapOpt {
    assertEquals(((@[@4, @2])), ([[[(@[@2, @0, @1]) chain] mapOpt:^id(id x) {
        if(unumi(x) == 0) return nil;
        else return numi(2 * unumi(x));
    }] toArray]));
}

- (void)testFlatMap {
    assertEquals(((@[@2, @4, @0, @0, @1, @2])), ([[[(@[@2, @0, @1]) chain] flatMap:^NSArray*(id x) {
        return (@[x, numi(2 * unumi(x))]);
    }] toArray]));
}

- (void)testFlat {
    assertEquals(((@[@1, @5, @2, @3, @2])), ([[[(@[((NSArray*)((@[@1, @5]))), ((NSArray*)((@[@2, @3]))), (@[@2])]) chain] flat] toArray]));
}

- (void)testZip {
    assertEquals(((@[@2, @3])), ([[[(@[@1, @0, @3]) chain] zipA:(@[@1, @3]) by:^id(id a, id b) {
        return numi(unumi(a) + unumi(b));
    }] toArray]));
}

- (void)testZip3 {
    assertEquals(((@[@3, @4])), ([[[(@[@1, @0, @3]) chain] zip3A:(@[@1, @3]) b:(@[@1, @1, @2, @4]) by:^id(id a, id b, id c) {
        return numi(unumi(a) + unumi(b) + unumi(c));
    }] toArray]));
}

- (void)testZipFor {
    __block NSArray* arr = (@[]);
    [[(@[@1, @0, @3]) chain] zipForA:(@[@1, @3]) by:^void(id a, id b) {
        arr = [arr addItem:numi(unumi(a) + unumi(b))];
    }];
    assertEquals(((@[@2, @3])), arr);
}

- (void)testAppend {
    assertEquals(((@[@1, @0, @2, @3, @1])), ([[[(@[@1, @0, @2]) chain] append:(@[@3, @1])] toArray]));
}

- (void)testPreppend {
    assertEquals(((@[@3, @1, @1, @0, @2])), ([[[(@[@1, @0, @2]) chain] prepend:(@[@3, @1])] toArray]));
}

- (void)testMul {
    assertEquals(((@[tuple(@1, @3), tuple(@1, @1), tuple(@0, @3), tuple(@0, @1), tuple(@2, @3), tuple(@2, @1)])), ([[[(@[@1, @0, @2]) chain] mul:(@[@3, @1])] toArray]));
}

- (void)testTop {
    assertEquals(((@[@1, @0])), ([[[(@[@1, @0, @2]) chain] topNumbers:2] toArray]));
    assertTrue(([[[[(@[@1, @0, @2]) chain] topNumbers:0] toArray] isEmpty]));
    assertEquals(((@[@1, @0, @2])), ([[[(@[@1, @0, @2]) chain] topNumbers:4] toArray]));
}

- (void)testSort {
    assertEquals(((@[@0, @1, @2])), ([[[(@[@1, @0, @2]) chain] sort] toArray]));
    assertEquals(((@[@2, @1, @0])), ([[[(@[@1, @0, @2]) chain] sortDesc] toArray]));
}

- (void)testReverse {
    assertEquals(((@[@2, @0, @1])), ([[[(@[@1, @0, @2]) chain] reverse] toArray]));
}

- (void)testGroupBy {
    assertEquals(([[(@[((NSArray*)((@[@1, @0]))), (@[@2])]) chain] toSet]), ([[[[(@[@1, @0, @2]) chain] groupBy:^id(id _) {
        return numb(unumi(_) <= 1);
    }] map:^NSArray*(CNTuple* _) {
        return ((CNTuple*)(_)).b;
    }] toSet]));
    assertEquals(([[(@[@-1, @3]) chain] toSet]), ([[[[(@[@1, @-2, @3]) chain] groupBy:^id(id _) {
        return numb(unumi(_) <= 1);
    } fold:^id(id a, id b) {
        return numi(unumi(a) + unumi(b));
    } withStart:^id() {
        return @0;
    }] map:^id(CNTuple* _) {
        return ((CNTuple*)(_)).b;
    }] toSet]));
}

- (void)testDistinct {
    assertEquals(((@[@1, @3, @4, @2])), ([[[(@[@1, @3, @1, @4, @4, @2]) chain] distinct] toArray]));
}

- (void)testCombinations {
    assertEquals(((@[tuple(@2, @0), tuple(@2, @1), tuple(@0, @1)])), ([[[(@[@2, @0, @1]) chain] combinations] toArray]));
}

- (void)testUncombinations {
    assertEquals(((@[@2, @0, @1])), ([[[(@[tuple(@2, @0), tuple(@2, @1), tuple(@0, @1)]) chain] uncombinations] toArray]));
}

- (void)testNeighbours {
    assertEquals(((@[tuple(@2, @0), tuple(@0, @1)])), ([[[(@[@2, @0, @1]) chain] neighbours] toArray]));
    assertEquals(((@[tuple(@2, @0), tuple(@0, @1), tuple(@1, @2)])), ([[[(@[@2, @0, @1]) chain] neighboursRing] toArray]));
}

- (CNClassType*)type {
    return [CNChainTest type];
}

+ (CNClassType*)type {
    return _CNChainTest_type;
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

