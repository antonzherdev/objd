#import "objd.h"
#import "CNTreeMapTest.h"

#import "CNTreeMap.h"
#import "CNChain.h"
#import "CNType.h"
@implementation CNTreeMapTest
static CNClassType* _CNTreeMapTest_type;

+ (instancetype)treeMapTest {
    return [[CNTreeMapTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapTest class]) _CNTreeMapTest_type = [CNClassType classTypeWithCls:[CNTreeMapTest class]];
}

- (void)testMain {
    CNMTreeMap* map = [CNMTreeMap apply];
    assertEquals(@0, numi(((NSInteger)([map count]))));
    assertTrue([map applyKey:@0] == nil);
    [map setKey:@0 value:@"test"];
    assertEquals(@"test", ((NSString*)(nonnil([map applyKey:@0]))));
    NSArray* tests = (@[@-10, @-20, @-30, @10, @20, @-15, @20, @0, @11, @13, @-18]);
    for(id i in tests) {
        [map setKey:i value:[@"test" stringByAppendingFormat:@"%ld", unumi(i)]];
    }
    assertEquals(numui([[[tests chain] distinct] count]), numui([map count]));
    [[[tests chain] distinct] forEach:^void(id i) {
        assertEquals(([@"test" stringByAppendingFormat:@"%ld", unumi(i)]), ((NSString*)(nonnil([map applyKey:i]))));
    }];
    assertEquals(((@[@-30, @-20, @-18, @-15, @-10, @0, @10, @11, @13, @20])), [[map.keys chain] toArray]);
    [[[tests chain] distinct] forEach:^void(id i) {
        assertEquals(([@"test" stringByAppendingFormat:@"%ld", unumi(i)]), ((NSString*)(nonnil([map applyKey:i]))));
        [map removeKey:i];
        assertTrue([map applyKey:i] == nil);
    }];
    assertEquals(@0, numi(((NSInteger)([map count]))));
}

- (CNClassType*)type {
    return [CNTreeMapTest type];
}

+ (CNClassType*)type {
    return _CNTreeMapTest_type;
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


