#import "objd.h"
#import "CNQueueTest.h"

#import "CNQueue.h"
#import "ODType.h"
@implementation CNQueueTest
static ODClassType* _CNQueueTest_type;

+ (id)queueTest {
    return [[CNQueueTest alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNQueueTest_type = [ODClassType classTypeWithCls:[CNQueueTest class]];
}

- (void)testDeque {
    CNQueue* q = [CNQueue apply];
    [self assertTrueValue:[q isEmpty]];
    [self assertEqualsA:@0 b:numi(((NSInteger)([q count])))];
    q = [q enqueueItem:@1];
    [self assertTrueValue:!([q isEmpty])];
    [self assertEqualsA:@1 b:numi(((NSInteger)([q count])))];
    q = [q enqueueItem:@2];
    [self assertEqualsA:@2 b:numi(((NSInteger)([q count])))];
    q = [q enqueueItem:@3];
    [self assertEqualsA:@3 b:numi(((NSInteger)([q count])))];
    CNTuple* p = [q dequeue];
    q = p.b;
    [self assertEqualsA:@1 b:[p.a get]];
    [self assertEqualsA:@2 b:numi(((NSInteger)([q count])))];
    p = [q dequeue];
    q = p.b;
    [self assertEqualsA:@2 b:[p.a get]];
    [self assertEqualsA:@1 b:numi(((NSInteger)([q count])))];
    p = [q dequeue];
    q = p.b;
    [self assertEqualsA:@3 b:[p.a get]];
    [self assertEqualsA:@0 b:numi(((NSInteger)([q count])))];
}

- (ODClassType*)type {
    return [CNQueueTest type];
}

+ (ODClassType*)type {
    return _CNQueueTest_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    return YES;
}

- (NSUInteger)hash {
    return 0;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


