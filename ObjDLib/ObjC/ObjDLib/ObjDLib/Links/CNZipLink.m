#import "objd.h"
#import "CNZipLink.h"

#import "CNCollection.h"
#import "CNYield.h"
#import "ODType.h"
@implementation CNZipLink
static ODClassType* _CNZipLink_type;
@synthesize a = _a;
@synthesize f = _f;

+ (instancetype)zipLinkWithA:(id<CNIterable>)a f:(id(^)(id, id))f {
    return [[CNZipLink alloc] initWithA:a f:f];
}

- (instancetype)initWithA:(id<CNIterable>)a f:(id(^)(id, id))f {
    self = [super init];
    if(self) {
        _a = a;
        _f = [f copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNZipLink class]) _CNZipLink_type = [ODClassType classTypeWithCls:[CNZipLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    id<CNIterator> ai = [_a iterator];
    return [CNYield decorateBase:yield yield:^NSInteger(id item) {
        if(!([ai hasNext])) return 1;
        else return [yield yieldItem:_f(item, [ai next])];
    }];
}

- (ODClassType*)type {
    return [CNZipLink type];
}

+ (ODClassType*)type {
    return _CNZipLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"a=%@", self.a];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNZip3Link
static ODClassType* _CNZip3Link_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize f = _f;

+ (instancetype)zip3LinkWithA:(id<CNIterable>)a b:(id<CNIterable>)b f:(id(^)(id, id, id))f {
    return [[CNZip3Link alloc] initWithA:a b:b f:f];
}

- (instancetype)initWithA:(id<CNIterable>)a b:(id<CNIterable>)b f:(id(^)(id, id, id))f {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
        _f = [f copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNZip3Link class]) _CNZip3Link_type = [ODClassType classTypeWithCls:[CNZip3Link class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    id<CNIterator> ai = [_a iterator];
    id<CNIterator> bi = [_b iterator];
    return [CNYield decorateBase:yield yield:^NSInteger(id item) {
        if(!([ai hasNext]) || !([bi hasNext])) return 1;
        else return [yield yieldItem:_f(item, [ai next], [bi next])];
    }];
}

- (ODClassType*)type {
    return [CNZip3Link type];
}

+ (ODClassType*)type {
    return _CNZip3Link_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"a=%@", self.a];
    [description appendFormat:@", b=%@", self.b];
    [description appendString:@">"];
    return description;
}

@end


