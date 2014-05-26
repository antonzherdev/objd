#import "objd.h"
#import "CNZipLink.h"

#import "CNType.h"
@implementation CNZipLink
static CNClassType* _CNZipLink_type;
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
    if(self == [CNZipLink class]) _CNZipLink_type = [CNClassType classTypeWithCls:[CNZipLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    id<CNIterator> ai = [_a iterator];
    return [CNYield decorateBase:yield yield:^CNGoR(id item) {
        if(!([ai hasNext])) return CNGo_Break;
        else return [yield yieldItem:_f(item, [ai next])];
    }];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"ZipLink(%@)", _a];
}

- (CNClassType*)type {
    return [CNZipLink type];
}

+ (CNClassType*)type {
    return _CNZipLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNZip3Link
static CNClassType* _CNZip3Link_type;
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
    if(self == [CNZip3Link class]) _CNZip3Link_type = [CNClassType classTypeWithCls:[CNZip3Link class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    id<CNIterator> ai = [_a iterator];
    id<CNIterator> bi = [_b iterator];
    return [CNYield decorateBase:yield yield:^CNGoR(id item) {
        if(!([ai hasNext]) || !([bi hasNext])) return CNGo_Break;
        else return [yield yieldItem:_f(item, [ai next], [bi next])];
    }];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Zip3Link(%@, %@)", _a, _b];
}

- (CNClassType*)type {
    return [CNZip3Link type];
}

+ (CNClassType*)type {
    return _CNZip3Link_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

