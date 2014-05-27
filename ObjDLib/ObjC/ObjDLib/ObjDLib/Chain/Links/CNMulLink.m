#import "CNMulLink.h"

#import "CNChain.h"
@implementation CNMulLink
static CNClassType* _CNMulLink_type;

+ (instancetype)mulLinkWithCollection:(id<CNTraversable>)collection {
    return [[CNMulLink alloc] initWithCollection:collection];
}

- (instancetype)initWithCollection:(id<CNTraversable>)collection {
    self = [super init];
    if(self) __collection = (([collection isKindOfClass:[CNChain class]]) ? ((id<CNTraversable>)([((CNChain*)(collection)) toArray])) : collection);
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMulLink class]) _CNMulLink_type = [CNClassType classTypeWithCls:[CNMulLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:({
        id<CNIterable> c = [CNObject asKindOfProtocol:@protocol(CNIterable) object:__collection];
        ((c != nil) ? ^CNGoR(NSUInteger size) {
            return [yield beginYieldWithSize:size * [((id<CNIterable>)(c)) count]];
        } : nil);
    }) yield:^CNGoR(id a) {
        return [__collection goOn:^CNGoR(id b) {
            return [yield yieldItem:tuple(a, b)];
        }];
    }];
}

- (NSString*)description {
    return @"MulLink";
}

- (CNClassType*)type {
    return [CNMulLink type];
}

+ (CNClassType*)type {
    return _CNMulLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

