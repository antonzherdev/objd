#import "CNActorTest.h"

#import "CNFuture.h"
#import "CNChain.h"
@implementation CNTestedActor
static CNClassType* _CNTestedActor_type;
@synthesize items = _items;

+ (instancetype)testedActor {
    return [[CNTestedActor alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _items = ((NSArray*)((@[])));
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTestedActor class]) _CNTestedActor_type = [CNClassType classTypeWithCls:[CNTestedActor class]];
}

- (CNFuture*)addNumber:(NSString*)number {
    return [self futureF:^id() {
        _items = [_items addItem:number];
        return nil;
    }];
}

- (CNFuture*)getItems {
    return [self promptF:^NSArray*() {
        return _items;
    }];
}

- (CNFuture*)getItemsF {
    return [self futureF:^NSArray*() {
        return _items;
    }];
}

- (CNFuture*)lockFuture:(CNFuture*)future {
    return [self lockAndOnSuccessFuture:future f:^NSString*(NSString* s) {
        _items = [_items addItem:[NSString stringWithFormat:@"w%@", s]];
        return s;
    }];
}

- (CNFuture*)lockVoidFuture:(CNFuture*)future {
    return [self lockAndOnSuccessFuture:future f:^id(NSString* s) {
        [self test];
        return nil;
    }];
}

- (void)test {
}

- (NSString*)description {
    return @"TestedActor";
}

- (CNClassType*)type {
    return [CNTestedActor type];
}

+ (CNClassType*)type {
    return _CNTestedActor_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNActorTest
static CNClassType* _CNActorTest_type;

+ (instancetype)actorTest {
    return [[CNActorTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNActorTest class]) _CNActorTest_type = [CNClassType classTypeWithCls:[CNActorTest class]];
}

- (void)testTypedActor {
    CNTestedActor* a = [CNTestedActor testedActor];
    NSArray* items = ((NSArray*)((@[])));
    NSInteger en = 0;
    cnLogInfoText(@"!!ADD");
    NSInteger count = 10000;
    {
        id<CNIterator> __il__5i = [intTo(1, count) iterator];
        while([__il__5i hasNext]) {
            id i = [__il__5i next];
            items = [items addItem:[NSString stringWithFormat:@"%@", i]];
        }
    }
    [[[[intTo(1, count) chain] mapF:^CNFuture*(id i) {
        return [CNFuture applyF:^id() {
            autoreleasePoolStart();
            [a addNumber:[NSString stringWithFormat:@"%@", i]];
            autoreleasePoolEnd();
            return nil;
        }];
    }] voidFuture] getResultAwait:5.0];
    cnLogInfoText(@"!!END_ADD");
    NSArray* result = [[a getItems] getResultAwait:5.0];
    NSArray* result2 = [[a getItemsF] getResultAwait:5.0];
    cnLogInfoText(@"!!GOT");
    assertEquals([[items chain] toSet], [[result chain] toSet]);
    assertEquals([[items chain] toSet], [[result2 chain] toSet]);
    assertTrue(en != count);
}

- (void)testTypedActor2 {
    [self repeatTimes:100 f:^void() {
        CNTestedActor* a = [CNTestedActor testedActor];
        NSArray* items = ((NSArray*)((@[])));
        NSInteger en = 0;
        cnLogInfoText(@"!!ADD");
        NSInteger count = 1000;
        {
            id<CNIterator> __il__0p1_5i = [intTo(1, count) iterator];
            while([__il__0p1_5i hasNext]) {
                id i = [__il__0p1_5i next];
                items = [items addItem:[NSString stringWithFormat:@"%@", i]];
            }
        }
        [[[[intTo(1, count) chain] mapF:^CNFuture*(id i) {
            return [a addNumber:[NSString stringWithFormat:@"%@", i]];
        }] voidFuture] getResultAwait:5.0];
        cnLogInfoText(@"!!END_ADD");
        NSArray* result = [[a getItems] getResultAwait:5.0];
        NSArray* result2 = [[a getItemsF] getResultAwait:5.0];
        cnLogInfoText(@"!!GOT");
        assertEquals([[items chain] toSet], [[result chain] toSet]);
        assertEquals([[items chain] toSet], [[result2 chain] toSet]);
        assertTrue(en != count);
    }];
}

- (void)testLock {
    [self repeatTimes:1000 f:^void() {
        CNTestedActor* a = [CNTestedActor testedActor];
        NSInteger count = 100;
        NSArray* arr = [[[intTo(1, count) chain] mapF:^CNTuple*(id _) {
            return tuple(_, [CNPromise apply]);
        }] toArray];
        for(CNTuple* t in arr) {
            [a lockFuture:((CNTuple*)(t)).b];
        }
        CNFuture* f = [a getItems];
        [[[arr chain] shuffle] forEach:^void(CNTuple* t) {
            [((CNPromise*)(((CNTuple*)(t)).b)) successValue:[NSString stringWithFormat:@"%@", ((CNTuple*)(t)).a]];
        }];
        NSArray* exp = [[[arr chain] mapF:^NSString*(CNTuple* _) {
            return [NSString stringWithFormat:@"w%@", ((CNTuple*)(_)).a];
        }] toArray];
        NSArray* items = [f getResultAwait:5.0];
        assertEquals(items, exp);
    }];
}

- (NSString*)description {
    return @"ActorTest";
}

- (CNClassType*)type {
    return [CNActorTest type];
}

+ (CNClassType*)type {
    return _CNActorTest_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

