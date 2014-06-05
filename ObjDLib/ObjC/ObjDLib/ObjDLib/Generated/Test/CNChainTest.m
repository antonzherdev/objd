#import "CNChainTest.h"

#import "CNChain.h"
#import "CNFuture.h"
#import "CNDispatchQueue.h"
#import "CNAtomic.h"
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
        NSArray* arr = [[[intTo(0, 1000) chain] mapF:^CNTuple*(id i) {
            return tuple(i, [CNPromise apply]);
        }] toArray];
        for(CNTuple* t in arr) {
            [[CNDispatchQueue aDefault] asyncF:^void() {
                [((CNPromise*)(((CNTuple*)(t))->_b)) successValue:numi(unumi(((CNTuple*)(t))->_a) * unumi(((CNTuple*)(t))->_a))];
            }];
        }
        CNFuture* fut = [[[arr chain] mapF:^CNPromise*(CNTuple* _) {
            return ((CNTuple*)(_))->_b;
        }] futureF:^NSArray*(CNChain* chain) {
            return [chain toArray];
        }];
        NSArray* set = [[[[arr chain] mapF:^id(CNTuple* _) {
            return ((CNTuple*)(_))->_a;
        }] mapF:^id(id _) {
            return numi(unumi(_) * unumi(_));
        }] toArray];
        assertEquals(set, [fut getResultAwait:5.0]);
    }];
}

- (void)testVoidFuture {
    NSArray* arr = [[[intTo(0, 1000) chain] mapF:^CNPromise*(id i) {
        return [CNPromise apply];
    }] toArray];
    CNFuture* fut = [[arr chain] voidFuture];
    CNAtomicInt* count = [CNAtomicInt atomicInt];
    for(CNPromise* p in arr) {
        [[CNDispatchQueue aDefault] asyncF:^void() {
            [count incrementAndGet];
            [((CNPromise*)(p)) successValue:nil];
        }];
    }
    assertTrue([fut waitResultPeriod:5.0] != nil);
    assertEquals(numi4([count intValue]), numi4(((int)([arr count]))));
}

- (void)testMap {
    assertEquals(((@[@4, @0, @2])), ([[[(@[@2, @0, @1]) chain] mapF:^id(id x) {
        return numi(2 * unumi(x));
    }] toArray]));
}

- (void)testMapOpt {
    assertEquals(((@[@4, @2])), ([[[(@[@2, @0, @1]) chain] mapOptF:^id(id x) {
        if(unumi(x) == 0) return nil;
        else return numi(2 * unumi(x));
    }] toArray]));
}

- (void)testFlatMap {
    assertEquals(((@[@2, @4, @0, @0, @1, @2])), ([[[(@[@2, @0, @1]) chain] flatMapF:^NSArray*(id x) {
        return (@[x, numi(2 * unumi(x))]);
    }] toArray]));
}

- (void)testFlat {
    assertEquals(((@[@1, @5, @2, @3, @2])), ([[[(@[((NSArray*)((@[@1, @5]))), ((NSArray*)((@[@2, @3]))), (@[@2])]) chain] flat] toArray]));
}

- (void)testZip {
    assertEquals(((@[@2, @3])), ([[[(@[@1, @0, @3]) chain] zipB:(@[@1, @3]) by:^id(id a, id b) {
        return numi(unumi(a) + unumi(b));
    }] toArray]));
}

- (void)testZip3 {
    assertEquals(((@[@3, @4])), ([[[(@[@1, @0, @3]) chain] zip3B:(@[@1, @3]) c:(@[@1, @1, @2, @4]) by:^id(id a, id b, id c) {
        return numi(unumi(a) + unumi(b) + unumi(c));
    }] toArray]));
}

- (void)testZipFor {
    __block NSArray* arr = ((NSArray*)((@[])));
    [[(@[@1, @0, @3]) chain] zipForB:(@[@1, @3]) by:^void(id a, id b) {
        arr = [arr addItem:numi(unumi(a) + unumi(b))];
    }];
    assertEquals(((@[@2, @3])), arr);
}

- (void)testAppend {
    assertEquals(((@[@1, @0, @2, @3, @1])), ([[[(@[@1, @0, @2]) chain] appendCollection:(@[@3, @1])] toArray]));
}

- (void)testPreppend {
    assertEquals(((@[@3, @1, @1, @0, @2])), ([[[(@[@1, @0, @2]) chain] prependCollection:(@[@3, @1])] toArray]));
}

- (void)testMul {
    assertEquals(((@[tuple(@1, @3), tuple(@1, @1), tuple(@0, @3), tuple(@0, @1), tuple(@2, @3), tuple(@2, @1)])), ([[[(@[@1, @0, @2]) chain] mulBy:(@[@3, @1])] toArray]));
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
    }] mapF:^NSArray*(CNTuple* _) {
        return ((CNTuple*)(_))->_b;
    }] toSet]));
    assertEquals(([[(@[@-1, @3]) chain] toSet]), ([[[[(@[@1, @-2, @3]) chain] groupBy:^id(id _) {
        return numb(unumi(_) <= 1);
    } start:^id() {
        return @0;
    } fold:^id(id a, id b) {
        return numi(unumi(a) + unumi(b));
    }] mapF:^id(CNTuple* _) {
        return ((CNTuple*)(_))->_b;
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

- (void)testExclude {
    assertEquals(((@[@0, @3, @0])), ([[[(@[@2, @0, @1, @2, @3, @0]) chain] excludeCollection:(@[@2, @1])] toArray]));
}

- (void)testIntersect {
    assertEquals(((@[@2, @1, @2])), ([[[(@[@2, @0, @1, @2, @3, @0]) chain] intersectCollection:(@[@2, @1])] toArray]));
}

- (void)testFold {
    assertEquals(@3, ([[(@[@2, @0, @1]) chain] foldStart:@0 by:^id(id r, id a) {
        return numi(unumi(r) + unumi(a));
    }]));
}

- (void)testCount {
    assertEquals(@3, (numi((((NSInteger)([[(@[@2, @0, @1]) chain] count]))))));
}

- (void)testHead {
    assertEquals(@2, (numi((unumi((nonnil(([[(@[@2, @0, @1]) chain] head]))))))));
}

- (void)testLast {
    assertEquals(@1, (numi((unumi((nonnil(([[(@[@2, @0, @1]) chain] last]))))))));
}

- (void)testRandom {
    assertTrue(([(@[@2, @0, @1]) containsItem:numi((unumi((nonnil(([[(@[@2, @0, @1]) chain] randomItem]))))))]));
}

- (void)testGap {
    assertEquals((tuple(@0, @2)), (((CNTuple*)(nonnil(([[(@[@2, @0, @1]) chain] gap]))))));
}

- (void)testMin {
    assertEquals(@0, (numi((unumi((nonnil(([[(@[@2, @0, @1]) chain] min]))))))));
}

- (void)testMax {
    assertEquals(@3, (numi((unumi((nonnil(([[(@[@2, @3, @0, @1]) chain] max]))))))));
}

- (void)testToString {
    assertEquals((@"2, 0, 1"), ([[(@[@2, @0, @1]) chain] toStringDelimiter:@", "]));
    assertEquals((@"[2, 0, 1]"), ([[(@[@2, @0, @1]) chain] toStringStart:@"[" delimiter:@", " end:@"]"]));
    assertEquals(@"201", [[@"201" chain] toString]);
}

- (NSString*)description {
    return @"ChainTest";
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

@end

