#import "objd.h"
#import "CNQueueTest.h"

#import "CNQueue.h"
#import "CNType.h"
@implementation CNQueueTest
static CNClassType* _CNQueueTest_type;

+ (instancetype)queueTest {
    return [[CNQueueTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNQueueTest class]) _CNQueueTest_type = [CNClassType classTypeWithCls:[CNQueueTest class]];
}

- (void)testDeque {
    CNImQueue* q = [CNImQueue apply];
    assertTrue([q isEmpty]);
    assertEquals(@0, numi(((NSInteger)([q count]))));
    q = [q enqueueItem:@1];
    assertTrue(!([q isEmpty]));
    assertEquals(@1, numi(((NSInteger)([q count]))));
    q = [q enqueueItem:@2];
    assertEquals(@2, numi(((NSInteger)([q count]))));
    q = [q enqueueItem:@3];
    assertEquals(@3, numi(((NSInteger)([q count]))));
    CNTuple* p = [q dequeue];
    q = p.b;
    assertEquals(@1, numi(unumi(nonnil(p.a))));
    assertEquals(@2, numi(((NSInteger)([q count]))));
    p = [q dequeue];
    q = p.b;
    assertEquals(@2, numi(unumi(nonnil(p.a))));
    assertEquals(@1, numi(((NSInteger)([q count]))));
    p = [q dequeue];
    q = p.b;
    assertEquals(@3, numi(unumi(nonnil(p.a))));
    assertEquals(@0, numi(((NSInteger)([q count]))));
}

- (CNClassType*)type {
    return [CNQueueTest type];
}

+ (CNClassType*)type {
    return _CNQueueTest_type;
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


