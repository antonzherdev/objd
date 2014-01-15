#import "objd.h"
#import "CNTreeSet.h"

#import "CNTreeMap.h"
#import "ODType.h"
#import "CNChain.h"
#import "ObjC.h"
@implementation CNTreeSet{
    CNTreeMap* _map;
}
static ODClassType* _CNTreeSet_type;
@synthesize map = _map;

+ (id)treeSetWithMap:(CNTreeMap*)map {
    return [[CNTreeSet alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeSet_type = [ODClassType classTypeWithCls:[CNTreeSet class]];
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
    return [[_map firstKey] get];
}

- (id)headOpt {
    return [_map firstKey];
}

- (id)last {
    return [_map lastKey];
}

- (BOOL)containsItem:(id)item {
    return [_map containsKey:item];
}

- (BOOL)isEmpty {
    return !([[self iterator] hasNext]);
}

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
    }
}

- (BOOL)goOn:(BOOL(^)(id))on {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if(!(on([i next]))) return NO;
    }
    return YES;
}

- (NSString*)description {
    return [[self chain] toStringWithStart:@"[" delimiter:@", " end:@"]"];
}

- (NSUInteger)hash {
    NSUInteger ret = 13;
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        ret = ret * 31 + [[i next] hash];
    }
    return ret;
}

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = [CNOption none];
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = [CNOption applyValue:x];
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = YES;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    __block BOOL ret = YES;
    [self goOn:^BOOL(id x) {
        if(!(confirm(x))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
}

- (ODClassType*)type {
    return [CNTreeSet type];
}

+ (ODClassType*)type {
    return _CNTreeSet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeSet* o = ((CNTreeSet*)(other));
    return [self.map isEqual:o.map];
}

@end


@implementation CNImTreeSet
static ODClassType* _CNImTreeSet_type;

+ (id)imTreeSetWithMap:(CNTreeMap*)map {
    return [[CNImTreeSet alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super initWithMap:map];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNImTreeSet_type = [ODClassType classTypeWithCls:[CNImTreeSet class]];
}

- (ODClassType*)type {
    return [CNImTreeSet type];
}

+ (ODClassType*)type {
    return _CNImTreeSet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNImTreeSet* o = ((CNImTreeSet*)(other));
    return [self.map isEqual:o.map];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.map hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"map=%@", self.map];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNTreeSetBuilder{
    NSInteger(^_comparator)(id, id);
    CNMTreeSet* _set;
}
static ODClassType* _CNTreeSetBuilder_type;
@synthesize comparator = _comparator;

+ (id)treeSetBuilderWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNTreeSetBuilder alloc] initWithComparator:comparator];
}

- (id)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) {
        _comparator = comparator;
        _set = [CNMTreeSet applyComparator:_comparator];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeSetBuilder_type = [ODClassType classTypeWithCls:[CNTreeSetBuilder class]];
}

+ (CNTreeSetBuilder*)apply {
    return [CNTreeSetBuilder treeSetBuilderWithComparator:^NSInteger(id a, id b) {
        return [a compareTo:b];
    }];
}

- (void)appendItem:(id)item {
    [_set appendItem:item];
}

- (CNTreeSet*)build {
    return _set;
}

- (void)appendAllItems:(id<CNTraversable>)items {
    [items forEach:^void(id _) {
        [self appendItem:_];
    }];
}

- (ODClassType*)type {
    return [CNTreeSetBuilder type];
}

+ (ODClassType*)type {
    return _CNTreeSetBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeSetBuilder* o = ((CNTreeSetBuilder*)(other));
    return [self.comparator isEqual:o.comparator];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.comparator hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNMTreeSet{
    CNMTreeMap* _mmap;
}
static NSObject* _CNMTreeSet_obj;
static ODClassType* _CNMTreeSet_type;
@synthesize mmap = _mmap;

+ (id)treeSetWithMmap:(CNMTreeMap*)mmap {
    return [[CNMTreeSet alloc] initWithMmap:mmap];
}

- (id)initWithMmap:(CNMTreeMap*)mmap {
    self = [super initWithMap:mmap];
    if(self) _mmap = mmap;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNMTreeSet_type = [ODClassType classTypeWithCls:[CNMTreeSet class]];
    _CNMTreeSet_obj = [NSObject object];
}

+ (CNMTreeSet*)applyComparator:(NSInteger(^)(id, id))comparator {
    return [CNMTreeSet treeSetWithMmap:[CNMTreeMap treeMapWithComparator:comparator]];
}

+ (CNMTreeSet*)apply {
    return [CNMTreeSet treeSetWithMmap:[CNMTreeMap apply]];
}

- (id<CNMutableIterator>)mutableIterator {
    return [_mmap.keys mutableIterator];
}

- (void)appendItem:(id)item {
    [_mmap setKey:item value:_CNMTreeSet_obj];
}

- (BOOL)removeItem:(id)item {
    return [[_mmap removeForKey:item] isDefined];
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

- (NSUInteger)count {
    id<CNIterator> i = [self iterator];
    NSUInteger n = 0;
    while([i hasNext]) {
        [i next];
        n++;
    }
    return n;
}

- (id)head {
    return [[self iterator] next];
}

- (id)headOpt {
    if([self isEmpty]) return [CNOption none];
    else return [CNOption applyValue:[self head]];
}

- (BOOL)isEmpty {
    return !([[self iterator] hasNext]);
}

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
    }
}

- (BOOL)goOn:(BOOL(^)(id))on {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if(!(on([i next]))) return NO;
    }
    return YES;
}

- (BOOL)containsItem:(id)item {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if([[i next] isEqual:i]) return YES;
    }
    return NO;
}

- (NSString*)description {
    return [[self chain] toStringWithStart:@"[" delimiter:@", " end:@"]"];
}

- (NSUInteger)hash {
    NSUInteger ret = 13;
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        ret = ret * 31 + [[i next] hash];
    }
    return ret;
}

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = [CNOption none];
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = [CNOption applyValue:x];
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = YES;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    __block BOOL ret = YES;
    [self goOn:^BOOL(id x) {
        if(!(confirm(x))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
}

- (ODClassType*)type {
    return [CNMTreeSet type];
}

+ (ODClassType*)type {
    return _CNMTreeSet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNMTreeSet* o = ((CNMTreeSet*)(other));
    return [self.mmap isEqual:o.mmap];
}

@end


