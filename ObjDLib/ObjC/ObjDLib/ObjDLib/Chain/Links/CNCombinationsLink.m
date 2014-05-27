#import "CNCombinationsLink.h"

@implementation CNCombinationsLink
static CNClassType* _CNCombinationsLink_type;

+ (instancetype)combinationsLink {
    return [[CNCombinationsLink alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNCombinationsLink class]) _CNCombinationsLink_type = [CNClassType classTypeWithCls:[CNCombinationsLink class]];
}

- (NSUInteger)sfN:(NSUInteger)n {
    NSInteger i = 1;
    NSInteger r = 0;
    while(i < n) {
        r += i;
        i++;
    }
    return ((NSUInteger)(r));
}

- (CNYield*)buildYield:(CNYield*)yield {
    __block CNMArray* prevs;
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        prevs = [CNMArray applyCapacity:size];
        return [yield beginYieldWithSize:[self sfN:size]];
    } yield:^CNGoR(id item) {
        CNGoR r = [((CNMArray*)(nonnil(prevs))) goOn:^CNGoR(id prev) {
            return [yield yieldItem:tuple(prev, item)];
        }];
        [((CNMArray*)(nonnil(prevs))) appendItem:item];
        return r;
    }];
}

- (NSString*)description {
    return @"CombinationsLink";
}

- (CNClassType*)type {
    return [CNCombinationsLink type];
}

+ (CNClassType*)type {
    return _CNCombinationsLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNUncombinationsLink
static CNClassType* _CNUncombinationsLink_type;

+ (instancetype)uncombinationsLink {
    return [[CNUncombinationsLink alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNUncombinationsLink class]) _CNUncombinationsLink_type = [CNClassType classTypeWithCls:[CNUncombinationsLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNMHashSet* set = [CNMHashSet hashSet];
    return [CNYield decorateBase:yield yield:^CNGoR(CNTuple* item) {
        CNGoR r = CNGo_Continue;
        id a = ((CNTuple*)(item)).a;
        if(!([set containsItem:a])) {
            [set appendItem:a];
            r = [yield yieldItem:a];
        }
        id b = ((CNTuple*)(item)).b;
        if(r == CNGo_Continue && !([set containsItem:b])) {
            [set appendItem:b];
            r = [yield yieldItem:b];
        }
        return r;
    }];
}

- (NSString*)description {
    return @"UncombinationsLink";
}

- (CNClassType*)type {
    return [CNUncombinationsLink type];
}

+ (CNClassType*)type {
    return _CNUncombinationsLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNNeighboursLink
static CNClassType* _CNNeighboursLink_type;
@synthesize ring = _ring;

+ (instancetype)neighboursLinkWithRing:(BOOL)ring {
    return [[CNNeighboursLink alloc] initWithRing:ring];
}

- (instancetype)initWithRing:(BOOL)ring {
    self = [super init];
    if(self) _ring = ring;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNNeighboursLink class]) _CNNeighboursLink_type = [CNClassType classTypeWithCls:[CNNeighboursLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    __block id first;
    __block id prev;
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:((size <= 1) ? 0 : ((_ring) ? size : size - 1))];
    } yield:^CNGoR(id item) {
        if(prev == nil) {
            first = item;
            prev = item;
            return CNGo_Continue;
        } else {
            id p = prev;
            prev = item;
            return [yield yieldItem:tuple(p, item)];
        }
    } end:((_ring) ? ^CNGoR(CNGoR result) {
        if(result == CNGo_Break || first == nil) return [yield endYieldWithResult:result];
        else return [yield endYieldWithResult:[yield yieldItem:tuple(((id)(prev)), ((id)(first)))]];
    } : nil)];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"NeighboursLink(%d)", _ring];
}

- (CNClassType*)type {
    return [CNNeighboursLink type];
}

+ (CNClassType*)type {
    return _CNNeighboursLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

