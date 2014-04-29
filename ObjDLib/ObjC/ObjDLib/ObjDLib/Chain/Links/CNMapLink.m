#import "objd.h"
#import "CNMapLink.h"

#import "CNYield.h"
#import "CNType.h"
@implementation CNMapLink
static CNClassType* _CNMapLink_type;
@synthesize f = _f;

+ (instancetype)mapLinkWithF:(id(^)(id))f {
    return [[CNMapLink alloc] initWithF:f];
}

- (instancetype)initWithF:(id(^)(id))f {
    self = [super init];
    if(self) _f = [f copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMapLink class]) _CNMapLink_type = [CNClassType classTypeWithCls:[CNMapLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield yield:^int(id item) {
        return [yield yieldItem:_f(item)];
    }];
}

- (CNClassType*)type {
    return [CNMapLink type];
}

+ (CNClassType*)type {
    return _CNMapLink_type;
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


@implementation CNMapOptLink
static CNClassType* _CNMapOptLink_type;
@synthesize f = _f;

+ (instancetype)mapOptLinkWithF:(id(^)(id))f {
    return [[CNMapOptLink alloc] initWithF:f];
}

- (instancetype)initWithF:(id(^)(id))f {
    self = [super init];
    if(self) _f = [f copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMapOptLink class]) _CNMapOptLink_type = [CNClassType classTypeWithCls:[CNMapOptLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield yield:^int(id item) {
        id __tmp_0;
        {
            id _ = _f(item);
            if(_ != nil) __tmp_0 = numi4([yield yieldItem:_]);
            else __tmp_0 = nil;
        }
        if(__tmp_0 != nil) return unumi4(__tmp_0);
        else return 0;
    }];
}

- (CNClassType*)type {
    return [CNMapOptLink type];
}

+ (CNClassType*)type {
    return _CNMapOptLink_type;
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


