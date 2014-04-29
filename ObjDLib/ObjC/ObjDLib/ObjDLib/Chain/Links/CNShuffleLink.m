#import "objd.h"
#import "CNShuffleLink.h"

#import "CNPlat.h"
#import "CNType.h"
@implementation CNShuffleLink
static CNClassType* _CNShuffleLink_type;

+ (instancetype)shuffleLink {
    return [[CNShuffleLink alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNShuffleLink class]) _CNShuffleLink_type = [CNClassType classTypeWithCls:[CNShuffleLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:^int(NSUInteger size) {
        __array = [CNMArray applyCapacity:size];
        return 0;
    } yield:^int(id item) {
        [((CNMArray*)(nonnil(__array))) insertIndex:cnuIntRndMax([((CNMArray*)(nonnil(__array))) count]) item:item];
        return 0;
    } end:^int(int r) {
        if([yield yieldAllItems:((CNMArray*)(nonnil(__array)))] == 1) return 1;
        else return r;
    }];
}

- (CNClassType*)type {
    return [CNShuffleLink type];
}

+ (CNClassType*)type {
    return _CNShuffleLink_type;
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


