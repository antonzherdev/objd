#import "objd.h"
#import "CNTreeSet.h"

#import "CNTreeMap.h"
#import "CNType.h"
#import "CNObject.h"
@implementation CNTreeSet
static CNClassType* _CNTreeSet_type;
@synthesize map = _map;

+ (instancetype)treeSetWithMap:(CNTreeMap*)map {
    return [[CNTreeSet alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeSet class]) _CNTreeSet_type = [CNClassType classTypeWithCls:[CNTreeSet class]];
}

- (id)higherThanItem:(id)item {
    return [_map higherKeyThanKey:item];
}

- (id)lowerThanItem:(id)item {
    return [_map lowerKeyThanKey:item];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [[_map keys] iterator];
}

- (id<CNIterator>)iteratorHigherThanItem:(id)item {
    return [[_map keys] iteratorHigherThanKey:item];
}

- (id)head {
    return [_map firstKey];
}

- (id)last {
    return [_map lastKey];
}

- (BOOL)containsItem:(id)item {
    return [_map containsKey:item];
}

- (CNClassType*)type {
    return [CNTreeSet type];
}

+ (CNClassType*)type {
    return _CNTreeSet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"map=%@", self.map];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNImTreeSet
static CNClassType* _CNImTreeSet_type;
@synthesize immap = _immap;

+ (instancetype)imTreeSetWithImmap:(CNImTreeMap*)immap {
    return [[CNImTreeSet alloc] initWithImmap:immap];
}

- (instancetype)initWithImmap:(CNImTreeMap*)immap {
    self = [super initWithMap:immap];
    if(self) _immap = immap;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImTreeSet class]) _CNImTreeSet_type = [CNClassType classTypeWithCls:[CNImTreeSet class]];
}

- (CNMTreeSet*)mCopy {
    return [CNMTreeSet treeSetWithMmap:[_immap mCopy]];
}

- (CNClassType*)type {
    return [CNImTreeSet type];
}

+ (CNClassType*)type {
    return _CNImTreeSet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"immap=%@", self.immap];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNTreeSetBuilder
static CNClassType* _CNTreeSetBuilder_type;
@synthesize comparator = _comparator;

+ (instancetype)treeSetBuilderWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNTreeSetBuilder alloc] initWithComparator:comparator];
}

- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) {
        _comparator = [comparator copy];
        _set = [CNMTreeSet applyComparator:comparator];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeSetBuilder class]) _CNTreeSetBuilder_type = [CNClassType classTypeWithCls:[CNTreeSetBuilder class]];
}

+ (CNTreeSetBuilder*)apply {
    return [CNTreeSetBuilder treeSetBuilderWithComparator:^NSInteger(id a, id b) {
        return [a compareTo:b];
    }];
}

- (void)appendItem:(id)item {
    [_set appendItem:item];
}

- (CNImTreeSet*)build {
    return [_set im];
}

- (CNClassType*)type {
    return [CNTreeSetBuilder type];
}

+ (CNClassType*)type {
    return _CNTreeSetBuilder_type;
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

@implementation CNMTreeSet
static NSObject* _CNMTreeSet_obj;
static CNClassType* _CNMTreeSet_type;
@synthesize mmap = _mmap;

+ (instancetype)treeSetWithMmap:(CNMTreeMap*)mmap {
    return [[CNMTreeSet alloc] initWithMmap:mmap];
}

- (instancetype)initWithMmap:(CNMTreeMap*)mmap {
    self = [super initWithMap:mmap];
    if(self) _mmap = mmap;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMTreeSet class]) {
        _CNMTreeSet_type = [CNClassType classTypeWithCls:[CNMTreeSet class]];
        _CNMTreeSet_obj = [CNObject object];
    }
}

+ (CNMTreeSet*)applyComparator:(NSInteger(^)(id, id))comparator {
    return [CNMTreeSet treeSetWithMmap:[CNMTreeMap treeMapWithComparator:comparator]];
}

+ (CNMTreeSet*)apply {
    return [CNMTreeSet treeSetWithMmap:[CNMTreeMap apply]];
}

- (id<CNMIterator>)mutableIterator {
    return [_mmap.keys mutableIterator];
}

- (void)appendItem:(id)item {
    [_mmap setKey:item value:_CNMTreeSet_obj];
}

- (BOOL)removeItem:(id)item {
    return [_mmap removeKey:item] != nil;
}

- (void)clear {
    [_mmap clear];
}

- (void)addAllObjects:(id<CNTraversable>)objects {
    [objects forEach:^void(id _) {
        [self appendItem:_];
    }];
}

- (CNMTreeSet*)reorder {
    CNMTreeSet* ret = [CNMTreeSet treeSetWithMmap:[CNMTreeMap treeMapWithComparator:_mmap.comparator]];
    [ret addAllObjects:self];
    return ret;
}

- (CNImTreeSet*)im {
    return [CNImTreeSet imTreeSetWithImmap:[_mmap im]];
}

- (CNImTreeSet*)imCopy {
    return [CNImTreeSet imTreeSetWithImmap:[_mmap imCopy]];
}

- (void)mutableFilterBy:(BOOL(^)(id))by {
    id<CNMIterator> i = [self mutableIterator];
    while([i hasNext]) {
        if(by([i next])) [i remove];
    }
}

- (CNClassType*)type {
    return [CNMTreeSet type];
}

+ (CNClassType*)type {
    return _CNMTreeSet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"mmap=%@", self.mmap];
    [description appendString:@">"];
    return description;
}

@end

