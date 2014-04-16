#import "objd.h"
#import "CNMapLink.h"

#import "CNYield.h"
#import "ODType.h"
@implementation CNMapLink
static ODClassType* _CNMapLink_type;
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
    if(self == [CNMapLink class]) _CNMapLink_type = [ODClassType classTypeWithCls:[CNMapLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield yield:^NSInteger(id item) {
        return [yield yieldItem:_f(item)];
    }];
}

- (ODClassType*)type {
    return [CNMapLink type];
}

+ (ODClassType*)type {
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
static ODClassType* _CNMapOptLink_type;
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
    if(self == [CNMapOptLink class]) _CNMapOptLink_type = [ODClassType classTypeWithCls:[CNMapOptLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield yield:^NSInteger(id item) {
        id __tmp_0;
        {
            id _ = _f(item);
            if(_ != nil) __tmp_0 = numi([yield yieldItem:_]);
            else __tmp_0 = nil;
        }
        if(__tmp_0 != nil) return unumi(__tmp_0);
        else return 0;
    }];
}

- (ODClassType*)type {
    return [CNMapOptLink type];
}

+ (ODClassType*)type {
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


