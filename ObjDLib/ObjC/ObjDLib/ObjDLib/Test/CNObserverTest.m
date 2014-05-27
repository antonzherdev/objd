#import "CNObserverTest.h"

#import "CNObserver.h"
@implementation CNObserverTest
static CNClassType* _CNObserverTest_type;

+ (instancetype)observerTest {
    return [[CNObserverTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNObserverTest class]) _CNObserverTest_type = [CNClassType classTypeWithCls:[CNObserverTest class]];
}

- (void)testSignal {
    CNSignal* sig = [CNSignal signal];
    __block NSInteger v = 0;
    CNObserver* o = [sig observeF:^void(id i) {
        v = unumi(i);
    }];
    assertEquals(numi(v), @0);
    [sig postData:@1];
    assertEquals(numi(v), @1);
    [sig postData:@2];
    assertEquals(numi(v), @2);
    [o detach];
    [sig postData:@3];
    assertEquals(numi(v), @2);
}

- (NSString*)description {
    return @"ObserverTest";
}

- (CNClassType*)type {
    return [CNObserverTest type];
}

+ (CNClassType*)type {
    return _CNObserverTest_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

