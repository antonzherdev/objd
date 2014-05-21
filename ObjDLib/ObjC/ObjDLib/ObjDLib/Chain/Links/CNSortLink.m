#import "objd.h"
#import "CNSortLink.h"

#import "CNTreeSet.h"
#import "CNType.h"
#import "CNMutableList.h"
#import "CNPlat.h"
@implementation CNSortLink
static CNClassType* _CNSortLink_type;
@synthesize comparator = _comparator;

+ (instancetype)sortLinkWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNSortLink alloc] initWithComparator:comparator];
}

- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) _comparator = [comparator copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSortLink class]) _CNSortLink_type = [CNClassType classTypeWithCls:[CNSortLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNMTreeSet* set = [CNMTreeSet applyComparator:_comparator];
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger _) {
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [set appendItem:item];
        return CNGo_Continue;
    } end:^CNGoR(CNGoR result) {
        if(result == CNGo_Break) return [yield endYieldWithResult:result];
        else return [yield yieldAllItems:set];
    }];
}

- (CNClassType*)type {
    return [CNSortLink type];
}

+ (CNClassType*)type {
    return _CNSortLink_type;
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

@implementation CNReverseLink
static CNClassType* _CNReverseLink_type;

+ (instancetype)reverseLink {
    return [[CNReverseLink alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNReverseLink class]) _CNReverseLink_type = [CNClassType classTypeWithCls:[CNReverseLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNMList* list = [CNMList list];
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger _) {
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [list prependItem:item];
        return CNGo_Continue;
    } end:^CNGoR(CNGoR result) {
        if(result == CNGo_Break) return [yield endYieldWithResult:result];
        else return [yield yieldAllItems:list];
    }];
}

- (CNClassType*)type {
    return [CNReverseLink type];
}

+ (CNClassType*)type {
    return _CNReverseLink_type;
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
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        __array = [CNMArray applyCapacity:size];
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((CNMArray*)(nonnil(__array))) insertIndex:cnuIntRndMax([((CNMArray*)(nonnil(__array))) count]) item:item];
        return CNGo_Continue;
    } end:^CNGoR(CNGoR r) {
        if(r == CNGo_Break) return [yield endYieldWithResult:r];
        else return [yield yieldAllItems:((CNMArray*)(nonnil(__array)))];
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

