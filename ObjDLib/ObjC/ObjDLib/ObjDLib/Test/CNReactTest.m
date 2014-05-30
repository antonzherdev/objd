#import "CNReactTest.h"

#import "CNReact.h"
@implementation CNReactTest
static CNClassType* _CNReactTest_type;

+ (instancetype)reactTest {
    return [[CNReactTest alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNReactTest class]) _CNReactTest_type = [CNClassType classTypeWithCls:[CNReactTest class]];
}

- (void)testMap {
    CNVar* v = [CNVar applyInitial:@2];
    CNReact* m = [v mapF:^id(id _) {
        return numi(unumi(_) * unumi(_));
    }];
    CNReact* m2 = [m mapF:^id(id _) {
        return numi(unumi(_) * unumi(_));
    }];
    assertEquals([m value], @4);
    assertEquals([m2 value], @16);
    [v setValue:@4];
    assertEquals([m value], @16);
    assertEquals([m2 value], numi(16 * 16));
}

- (void)testReactFlag {
    CNVar* a1 = [CNVar applyInitial:@1];
    CNVar* a2 = [CNVar applyInitial:@2];
    CNReactFlag* f = [CNReactFlag reactFlagWithInitial:YES reacts:(@[a1, a2])];
    assertTrue(unumb([f value]));
    [f clear];
    assertFalse(unumb([f value]));
    [f set];
    assertTrue(unumb([f value]));
    [f clear];
    assertFalse(unumb([f value]));
    [a1 setValue:@1];
    assertFalse(unumb([f value]));
    [a1 setValue:@2];
    assertTrue(unumb([f value]));
    [f clear];
    assertFalse(unumb([f value]));
    [a2 setValue:@1];
    assertTrue(unumb([f value]));
    [f clear];
    assertFalse(unumb([f value]));
    [a1 setValue:@3];
    [a2 setValue:@3];
    assertTrue(unumb([f value]));
}

- (NSString*)description {
    return @"ReactTest";
}

- (CNClassType*)type {
    return [CNReactTest type];
}

+ (CNClassType*)type {
    return _CNReactTest_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

