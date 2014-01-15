#import "objd.h"
#import "CNTreeMap.h"

#import "CNOption.h"
#import "ODType.h"
#import "CNChain.h"
@implementation CNTreeMap{
    NSInteger(^_comparator)(id, id);
    CNTreeMapValues* _values;
}
static NSInteger _CNTreeMap_BLACK = 0;
static NSInteger _CNTreeMap_RED = 1;
static ODClassType* _CNTreeMap_type;
@synthesize comparator = _comparator;
@synthesize values = _values;

+ (id)treeMapWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNTreeMap alloc] initWithComparator:comparator];
}

- (id)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) {
        _comparator = comparator;
        _values = [CNTreeMapValues treeMapValuesWithMap:self];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMap_type = [ODClassType classTypeWithCls:[CNTreeMap class]];
}

- (id)applyKey:(id)key {
    return [self entryForKey:key].value;
}

- (id)optKey:(id)key {
    CNTreeMapEntry* e = [self entryForKey:key];
    if(e == nil) return [CNOption none];
    else return [CNOption applyValue:e.value];
}

- (CNTreeMapEntry*)root {
    @throw @"Method root is abstract";
}

- (BOOL)isEmpty {
    return [self root] == nil;
}

- (CNTreeMapEntry*)entryForKey:(id)key {
    CNTreeMapEntry* p = [self root];
    while(p != nil) {
        NSInteger cmp = _comparator(key, p.key);
        if(cmp < 0) {
            p = p.left;
        } else {
            if(cmp > 0) p = p.right;
            else break;
        }
    }
    return p;
}

- (id<CNTreeMapKeySet>)keys {
    @throw @"Method keys is abstract";
}

- (id<CNIterator>)iterator {
    return [CNTreeMapIterator applyMap:self entry:[self firstEntry]];
}

- (CNTreeMapIterator*)iteratorHigherThanKey:(id)key {
    return [CNTreeMapIterator applyMap:self entry:[[self higherEntryThanKey:key] getOrValue:nil]];
}

- (CNTreeMapEntry*)firstEntry {
    CNTreeMapEntry* p = [self root];
    if(p != nil) while(p.left != nil) {
        p = p.left;
    }
    return p;
}

- (id)firstKey {
    if([self root] == nil) return [CNOption none];
    else return [CNOption applyValue:[self firstEntry].key];
}

- (id)lastKey {
    if([self root] == nil) return [CNOption none];
    else return [CNOption applyValue:[self lastEntry].key];
}

- (id)lowerKeyThanKey:(id)key {
    return [[self lowerEntryThanKey:key] mapF:^id(CNTreeMapEntry* _) {
        return ((CNTreeMapEntry*)(_)).key;
    }];
}

- (id)higherKeyThanKey:(id)key {
    return [[self higherEntryThanKey:key] mapF:^id(CNTreeMapEntry* _) {
        return ((CNTreeMapEntry*)(_)).key;
    }];
}

- (id)lowerEntryThanKey:(id)key {
    CNTreeMapEntry* p = [self root];
    while(p != nil) {
        NSInteger cmp = _comparator(key, p.key);
        if(cmp > 0) {
            if(p.right != nil) p = p.right;
            else return [CNSome someWithValue:p];
        } else {
            if(p.left != nil) {
                p = p.left;
            } else {
                CNTreeMapEntry* parent = p.parent;
                CNTreeMapEntry* ch = p;
                while(parent != nil && ch == parent.left) {
                    ch = parent;
                    parent = parent.parent;
                }
                return [CNSome someWithValue:parent];
            }
        }
    }
    return [CNOption none];
}

- (id)higherEntryThanKey:(id)key {
    CNTreeMapEntry* p = [self root];
    while(p != nil) {
        NSInteger cmp = _comparator(key, p.key);
        if(cmp < 0) {
            if(p.left != nil) p = p.left;
            else return [CNSome someWithValue:p];
        } else {
            if(p.right != nil) {
                p = p.right;
            } else {
                CNTreeMapEntry* parent = p.parent;
                CNTreeMapEntry* ch = p;
                while(parent != nil && ch == parent.right) {
                    ch = parent;
                    parent = parent.parent;
                }
                return [CNSome someWithValue:parent];
            }
        }
    }
    return [CNOption none];
}

- (CNTreeMapEntry*)lastEntry {
    CNTreeMapEntry* p = [self root];
    if(p != nil) while(p.right != nil) {
        p = p.right;
    }
    return p;
}

- (BOOL)containsKey:(id)key {
    return [[self optKey:key] isDefined];
}

- (BOOL)isValueEqualKey:(id)key value:(id)value {
    id v = [self optKey:key];
    if([v isEmpty]) return NO;
    else return [value isEqual:[v get]];
}

- (id<CNMap>)addItem:(CNTuple*)item {
    CNHashMapBuilder* builder = [CNHashMapBuilder hashMapBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
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
    return [CNTreeMap type];
}

+ (NSInteger)BLACK {
    return _CNTreeMap_BLACK;
}

+ (NSInteger)RED {
    return _CNTreeMap_RED;
}

+ (ODClassType*)type {
    return _CNTreeMap_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeMap* o = ((CNTreeMap*)(other));
    return [self.comparator isEqual:o.comparator];
}

@end


@implementation CNImTreeMap{
    CNTreeMapEntry* _root;
    NSUInteger _count;
    id<CNTreeMapKeySet> _keys;
}
static ODClassType* _CNImTreeMap_type;
@synthesize root = _root;
@synthesize count = _count;
@synthesize keys = _keys;

+ (id)imTreeMapWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count {
    return [[CNImTreeMap alloc] initWithComparator:comparator root:root count:count];
}

- (id)initWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count {
    self = [super initWithComparator:comparator];
    if(self) {
        _root = root;
        _count = count;
        _keys = [CNImTreeMapKeySet imTreeMapKeySetWithMap:self];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNImTreeMap_type = [ODClassType classTypeWithCls:[CNImTreeMap class]];
}

- (BOOL)isEmpty {
    return _root == nil;
}

- (ODClassType*)type {
    return [CNImTreeMap type];
}

+ (ODClassType*)type {
    return _CNImTreeMap_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNImTreeMap* o = ((CNImTreeMap*)(other));
    return [self.comparator isEqual:o.comparator] && self.root == o.root && self.count == o.count;
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.comparator hash];
    hash = hash * 31 + [self.root hash];
    hash = hash * 31 + self.count;
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"root=%@", self.root];
    [description appendFormat:@", count=%lu", (unsigned long)self.count];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNTreeMapBuilder{
    NSInteger(^_comparator)(id, id);
    CNMTreeMap* _map;
}
static ODClassType* _CNTreeMapBuilder_type;
@synthesize comparator = _comparator;

+ (id)treeMapBuilderWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNTreeMapBuilder alloc] initWithComparator:comparator];
}

- (id)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) {
        _comparator = comparator;
        _map = [CNMTreeMap treeMapWithComparator:_comparator];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMapBuilder_type = [ODClassType classTypeWithCls:[CNTreeMapBuilder class]];
}

+ (CNTreeMapBuilder*)apply {
    return [CNTreeMapBuilder treeMapBuilderWithComparator:^NSInteger(id a, id b) {
        return [a compareTo:b];
    }];
}

- (void)appendItem:(CNTuple*)item {
    [_map appendItem:item];
}

- (CNTreeMap*)build {
    return _map;
}

- (void)appendAllItems:(id<CNTraversable>)items {
    [items forEach:^void(id _) {
        [self appendItem:_];
    }];
}

- (ODClassType*)type {
    return [CNTreeMapBuilder type];
}

+ (ODClassType*)type {
    return _CNTreeMapBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeMapBuilder* o = ((CNTreeMapBuilder*)(other));
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


@implementation CNMTreeMap{
    CNTreeMapEntry* __root;
    NSUInteger __size;
    CNMTreeMapKeySet* _keys;
}
static ODClassType* _CNMTreeMap_type;
@synthesize keys = _keys;

+ (id)treeMapWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNMTreeMap alloc] initWithComparator:comparator];
}

- (id)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super initWithComparator:comparator];
    if(self) {
        __root = nil;
        __size = 0;
        _keys = [CNMTreeMapKeySet treeMapKeySetWithMap:self];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNMTreeMap_type = [ODClassType classTypeWithCls:[CNMTreeMap class]];
}

+ (CNMTreeMap*)apply {
    return [CNMTreeMap treeMapWithComparator:^NSInteger(id a, id b) {
        return [a compareTo:b];
    }];
}

- (CNTreeMapEntry*)root {
    return __root;
}

- (NSUInteger)count {
    return __size;
}

- (void)clear {
    __size = 0;
    __root = nil;
}

- (id<CNMutableIterator>)mutableIterator {
    return [CNMTreeMapIterator applyMap:self entry:[self firstEntry]];
}

- (void)setKey:(id)key value:(id)value {
    NSInteger(^_comparator)(id, id) = self.comparator;
    CNTreeMapEntry* t = __root;
    if(t == nil) {
        __root = [CNTreeMapEntry newWithKey:key value:value parent:nil];
        __size = 1;
    } else {
        NSInteger cmp = 0;
        CNTreeMapEntry* parent = nil;
        do {
            parent = t;
            cmp = _comparator(key, t.key);
            if(cmp < 0) {
                t = t.left;
            } else {
                if(cmp > 0) {
                    t = t.right;
                } else {
                    t.value = value;
                    return ;
                }
            }
        } while(t != nil);
        CNTreeMapEntry* e = [CNTreeMapEntry newWithKey:key value:value parent:parent];
        if(cmp < 0) parent.left = e;
        else parent.right = e;
        [self fixAfterInsertionEntry:e];
        __size++;
    }
}

- (id)removeForKey:(id)key {
    CNTreeMapEntry* entry = [self entryForKey:key];
    if(entry != nil) return [CNOption applyValue:[self deleteEntry:entry]];
    else return [CNOption none];
}

- (id)deleteEntry:(CNTreeMapEntry*)entry {
    CNTreeMapEntry* p = entry;
    __size--;
    if(p.left != nil && p.right != nil) {
        CNTreeMapEntry* s = [p next];
        p.key = s.key;
        p.value = s.value;
        p = s;
    }
    CNTreeMapEntry* replacement = ((p.left != nil) ? p.left : p.right);
    if(replacement != nil) {
        replacement.parent = p.parent;
        if(p.parent == nil) {
            __root = replacement;
        } else {
            if(p == p.parent.left) p.parent.left = replacement;
            else p.parent.right = replacement;
        }
        p.left = nil;
        p.right = nil;
        p.parent = nil;
        if(p.color == [CNMTreeMap BLACK]) [self fixAfterDeletionEntry:replacement];
    } else {
        if(p.parent == nil) {
            __root = nil;
        } else {
            if(p.color == [CNMTreeMap BLACK]) [self fixAfterDeletionEntry:p];
            if(p.parent != nil) {
                if(p == p.parent.left) {
                    p.parent.left = nil;
                } else {
                    if(p == p.parent.right) p.parent.right = nil;
                }
                p.parent = nil;
            }
        }
    }
    return entry.value;
}

- (void)fixAfterInsertionEntry:(CNTreeMapEntry*)entry {
    CNTreeMapEntry* x = entry;
    x.color = [CNMTreeMap RED];
    while(x != nil && x != __root && x.parent.color == [CNMTreeMap RED]) {
        if(x.parent == x.parent.parent.left) {
            CNTreeMapEntry* y = x.parent.parent.right;
            if(y.color == [CNMTreeMap RED]) {
                x.parent.color = [CNMTreeMap BLACK];
                y.color = [CNMTreeMap BLACK];
                x.parent.parent.color = [CNMTreeMap RED];
                x = x.parent.parent;
            } else {
                if(x == x.parent.right) {
                    x = x.parent;
                    [self rotateLeftP:x];
                }
                x.parent.color = [CNMTreeMap BLACK];
                x.parent.parent.color = [CNMTreeMap RED];
                [self rotateRightP:x.parent.parent];
            }
        } else {
            CNTreeMapEntry* y = x.parent.parent.left;
            if(y.color == [CNMTreeMap RED]) {
                x.parent.color = [CNMTreeMap BLACK];
                y.color = [CNMTreeMap BLACK];
                x.parent.parent.color = [CNMTreeMap RED];
                x = x.parent.parent;
            } else {
                if(x == x.parent.left) {
                    x = x.parent;
                    [self rotateRightP:x];
                }
                x.parent.color = [CNMTreeMap BLACK];
                x.parent.parent.color = [CNMTreeMap RED];
                [self rotateLeftP:x.parent.parent];
            }
        }
    }
    __root.color = [CNMTreeMap BLACK];
}

- (void)fixAfterDeletionEntry:(CNTreeMapEntry*)entry {
    CNTreeMapEntry* x = entry;
    while(x != __root && x.color == [CNMTreeMap BLACK]) {
        if(x == x.parent.left) {
            CNTreeMapEntry* sib = x.parent.right;
            if(sib.color == [CNMTreeMap RED]) {
                sib.color = [CNMTreeMap BLACK];
                x.parent.color = [CNMTreeMap RED];
                [self rotateLeftP:x.parent];
                sib = x.parent.right;
            }
            if(sib.left.color == [CNMTreeMap BLACK] && sib.right.color == [CNMTreeMap BLACK]) {
                sib.color = [CNMTreeMap RED];
                x = x.parent;
            } else {
                if(sib.right.color == [CNMTreeMap BLACK]) {
                    sib.left.color = [CNMTreeMap BLACK];
                    sib.color = [CNMTreeMap RED];
                    [self rotateRightP:sib];
                    sib = x.parent.right;
                }
                sib.color = x.parent.color;
                x.parent.color = [CNMTreeMap BLACK];
                sib.right.color = [CNMTreeMap BLACK];
                [self rotateLeftP:x.parent];
                x = __root;
            }
        } else {
            CNTreeMapEntry* sib = x.parent.left;
            if(sib.color == [CNMTreeMap RED]) {
                sib.color = [CNMTreeMap BLACK];
                x.parent.color = [CNMTreeMap RED];
                [self rotateRightP:x.parent];
                sib = x.parent.left;
            }
            if(sib.right.color == [CNMTreeMap BLACK] && sib.left.color == [CNMTreeMap BLACK]) {
                sib.color = [CNMTreeMap RED];
                x = x.parent;
            } else {
                if(sib.left.color == [CNMTreeMap BLACK]) {
                    sib.right.color = [CNMTreeMap BLACK];
                    sib.color = [CNMTreeMap RED];
                    [self rotateLeftP:sib];
                    sib = x.parent.left;
                }
                sib.color = x.parent.color;
                x.parent.color = [CNMTreeMap BLACK];
                sib.left.color = [CNMTreeMap BLACK];
                [self rotateRightP:x.parent];
                x = __root;
            }
        }
    }
    x.color = [CNMTreeMap BLACK];
}

- (void)rotateLeftP:(CNTreeMapEntry*)p {
    if(p != nil) {
        CNTreeMapEntry* r = p.right;
        p.right = r.left;
        if(r.left != nil) r.left.parent = p;
        r.parent = p.parent;
        if(p.parent == nil) {
            __root = r;
        } else {
            if(p.parent.left == p) p.parent.left = r;
            else p.parent.right = r;
        }
        r.left = p;
        p.parent = r;
    }
}

- (void)rotateRightP:(CNTreeMapEntry*)p {
    if(p != nil) {
        CNTreeMapEntry* l = p.left;
        p.left = l.right;
        if(l.right != nil) l.right.parent = p;
        l.parent = p.parent;
        if(p.parent == nil) {
            __root = l;
        } else {
            if(p.parent.right == p) p.parent.right = l;
            else p.parent.left = l;
        }
        l.right = p;
        p.parent = l;
    }
}

- (id)pollFirst {
    CNTreeMapEntry* entry = [self firstEntry];
    if(entry == nil) {
        return [CNOption none];
    } else {
        [self deleteEntry:entry];
        return [CNOption applyValue:tuple(entry.key, entry.value)];
    }
}

- (id)objectForKey:(id)key orUpdateWith:(id(^)())orUpdateWith {
    id o = [self optKey:key];
    if([o isDefined]) {
        return [o get];
    } else {
        id init = ((id(^)())(orUpdateWith))();
        [self setKey:key value:init];
        return init;
    }
}

- (id)modifyBy:(id(^)(id))by forKey:(id)forKey {
    id newObject = by([CNOption applyValue:[self applyKey:forKey]]);
    if([newObject isEmpty]) [self removeForKey:forKey];
    else [self setKey:forKey value:[newObject get]];
    return newObject;
}

- (void)appendItem:(CNTuple*)item {
    [self setKey:item.b value:item.a];
}

- (void)removeItem:(CNTuple*)item {
    [self removeForKey:item.a];
}

- (BOOL)containsKey:(id)key {
    return [[self optKey:key] isDefined];
}

- (BOOL)isValueEqualKey:(id)key value:(id)value {
    id v = [self optKey:key];
    if([v isEmpty]) return NO;
    else return [value isEqual:[v get]];
}

- (id<CNMap>)addItem:(CNTuple*)item {
    CNHashMapBuilder* builder = [CNHashMapBuilder hashMapBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
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
    return [CNMTreeMap type];
}

+ (ODClassType*)type {
    return _CNMTreeMap_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNMTreeMap* o = ((CNMTreeMap*)(other));
    return [self.comparator isEqual:o.comparator];
}

@end


@implementation CNTreeMapEntry{
    id _key;
    id _value;
    CNTreeMapEntry* _left;
    CNTreeMapEntry* _right;
    NSInteger _color;
    __weak CNTreeMapEntry* _parent;
}
static ODClassType* _CNTreeMapEntry_type;
@synthesize key = _key;
@synthesize value = _value;
@synthesize left = _left;
@synthesize right = _right;
@synthesize color = _color;
@synthesize parent = _parent;

+ (id)treeMapEntry {
    return [[CNTreeMapEntry alloc] init];
}

- (id)init {
    self = [super init];
    if(self) {
        _left = nil;
        _right = nil;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMapEntry_type = [ODClassType classTypeWithCls:[CNTreeMapEntry class]];
}

+ (CNTreeMapEntry*)newWithKey:(id)key value:(id)value parent:(CNTreeMapEntry*)parent {
    CNTreeMapEntry* r = [CNTreeMapEntry treeMapEntry];
    r.key = key;
    r.value = value;
    r.parent = parent;
    return r;
}

- (CNTreeMapEntry*)next {
    if(_right != nil) {
        CNTreeMapEntry* p = _right;
        while(p.left != nil) {
            p = p.left;
        }
        return p;
    } else {
        CNTreeMapEntry* p = _parent;
        CNTreeMapEntry* ch = self;
        while(p != nil && ch == p.right) {
            ch = p;
            p = p.parent;
        }
        return p;
    }
}

- (ODClassType*)type {
    return [CNTreeMapEntry type];
}

+ (ODClassType*)type {
    return _CNTreeMapEntry_type;
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


@implementation CNImTreeMapKeySet{
    __weak CNTreeMap* _map;
}
static ODClassType* _CNImTreeMapKeySet_type;
@synthesize map = _map;

+ (id)imTreeMapKeySetWithMap:(CNTreeMap*)map {
    return [[CNImTreeMapKeySet alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNImTreeMapKeySet_type = [ODClassType classTypeWithCls:[CNImTreeMapKeySet class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [CNTreeMapKeyIterator applyMap:_map entry:[_map firstEntry]];
}

- (id<CNIterator>)iteratorHigherThanKey:(id)key {
    return [CNTreeMapKeyIterator applyMap:_map entry:[[_map higherEntryThanKey:key] getOrValue:nil]];
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
    return [CNImTreeMapKeySet type];
}

+ (ODClassType*)type {
    return _CNImTreeMapKeySet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNImTreeMapKeySet* o = ((CNImTreeMapKeySet*)(other));
    return [self.map isEqual:o.map];
}

@end


@implementation CNTreeMapKeyIterator{
    CNTreeMap* _map;
    CNTreeMapEntry* _entry;
}
static ODClassType* _CNTreeMapKeyIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (id)treeMapKeyIteratorWithMap:(CNTreeMap*)map {
    return [[CNTreeMapKeyIterator alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMapKeyIterator_type = [ODClassType classTypeWithCls:[CNTreeMapKeyIterator class]];
}

+ (CNTreeMapKeyIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNTreeMapKeyIterator* ret = [CNTreeMapKeyIterator treeMapKeyIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (id)next {
    id ret = _entry.key;
    _entry = [_entry next];
    return ret;
}

- (ODClassType*)type {
    return [CNTreeMapKeyIterator type];
}

+ (ODClassType*)type {
    return _CNTreeMapKeyIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeMapKeyIterator* o = ((CNTreeMapKeyIterator*)(other));
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


@implementation CNMTreeMapKeySet{
    __weak CNMTreeMap* _map;
}
static ODClassType* _CNMTreeMapKeySet_type;
@synthesize map = _map;

+ (id)treeMapKeySetWithMap:(CNMTreeMap*)map {
    return [[CNMTreeMapKeySet alloc] initWithMap:map];
}

- (id)initWithMap:(CNMTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNMTreeMapKeySet_type = [ODClassType classTypeWithCls:[CNMTreeMapKeySet class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [CNTreeMapKeyIterator applyMap:_map entry:[_map firstEntry]];
}

- (id<CNMutableIterator>)mutableIterator {
    return [CNMTreeMapKeyIterator applyMap:_map entry:[_map firstEntry]];
}

- (id<CNIterator>)iteratorHigherThanKey:(id)key {
    return [CNMTreeMapKeyIterator applyMap:_map entry:[[_map higherEntryThanKey:key] getOrValue:nil]];
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
    return [CNMTreeMapKeySet type];
}

+ (ODClassType*)type {
    return _CNMTreeMapKeySet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNMTreeMapKeySet* o = ((CNMTreeMapKeySet*)(other));
    return [self.map isEqual:o.map];
}

@end


@implementation CNMTreeMapKeyIterator{
    CNMTreeMap* _map;
    CNTreeMapEntry* _prev;
    CNTreeMapEntry* _entry;
}
static ODClassType* _CNMTreeMapKeyIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (id)treeMapKeyIteratorWithMap:(CNMTreeMap*)map {
    return [[CNMTreeMapKeyIterator alloc] initWithMap:map];
}

- (id)initWithMap:(CNMTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNMTreeMapKeyIterator_type = [ODClassType classTypeWithCls:[CNMTreeMapKeyIterator class]];
}

+ (CNMTreeMapKeyIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNMTreeMapKeyIterator* ret = [CNMTreeMapKeyIterator treeMapKeyIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (id)next {
    id ret = _entry.key;
    _prev = _entry;
    _entry = [_entry next];
    return ret;
}

- (void)remove {
    [_map deleteEntry:_prev];
}

- (ODClassType*)type {
    return [CNMTreeMapKeyIterator type];
}

+ (ODClassType*)type {
    return _CNMTreeMapKeyIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNMTreeMapKeyIterator* o = ((CNMTreeMapKeyIterator*)(other));
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


@implementation CNTreeMapValues{
    __weak CNTreeMap* _map;
}
static ODClassType* _CNTreeMapValues_type;
@synthesize map = _map;

+ (id)treeMapValuesWithMap:(CNTreeMap*)map {
    return [[CNTreeMapValues alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMapValues_type = [ODClassType classTypeWithCls:[CNTreeMapValues class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [CNTreeMapValuesIterator applyMap:_map entry:[_map firstEntry]];
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
    return [CNTreeMapValues type];
}

+ (ODClassType*)type {
    return _CNTreeMapValues_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeMapValues* o = ((CNTreeMapValues*)(other));
    return [self.map isEqual:o.map];
}

@end


@implementation CNTreeMapValuesIterator{
    CNTreeMap* _map;
    CNTreeMapEntry* _entry;
}
static ODClassType* _CNTreeMapValuesIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (id)treeMapValuesIteratorWithMap:(CNTreeMap*)map {
    return [[CNTreeMapValuesIterator alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMapValuesIterator_type = [ODClassType classTypeWithCls:[CNTreeMapValuesIterator class]];
}

+ (CNTreeMapValuesIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNTreeMapValuesIterator* ret = [CNTreeMapValuesIterator treeMapValuesIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (id)next {
    id ret = _entry.value;
    _entry = [_entry next];
    return ret;
}

- (ODClassType*)type {
    return [CNTreeMapValuesIterator type];
}

+ (ODClassType*)type {
    return _CNTreeMapValuesIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeMapValuesIterator* o = ((CNTreeMapValuesIterator*)(other));
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


@implementation CNTreeMapIterator{
    CNTreeMap* _map;
    CNTreeMapEntry* _entry;
}
static ODClassType* _CNTreeMapIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (id)treeMapIteratorWithMap:(CNTreeMap*)map {
    return [[CNTreeMapIterator alloc] initWithMap:map];
}

- (id)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTreeMapIterator_type = [ODClassType classTypeWithCls:[CNTreeMapIterator class]];
}

+ (CNTreeMapIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNTreeMapIterator* ret = [CNTreeMapIterator treeMapIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (id)next {
    CNTuple* ret = tuple(_entry.key, _entry.value);
    _entry = [_entry next];
    return ret;
}

- (ODClassType*)type {
    return [CNTreeMapIterator type];
}

+ (ODClassType*)type {
    return _CNTreeMapIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTreeMapIterator* o = ((CNTreeMapIterator*)(other));
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


@implementation CNMTreeMapIterator{
    CNMTreeMap* _map;
    CNTreeMapEntry* _prev;
    CNTreeMapEntry* _entry;
}
static ODClassType* _CNMTreeMapIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (id)treeMapIteratorWithMap:(CNMTreeMap*)map {
    return [[CNMTreeMapIterator alloc] initWithMap:map];
}

- (id)initWithMap:(CNMTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNMTreeMapIterator_type = [ODClassType classTypeWithCls:[CNMTreeMapIterator class]];
}

+ (CNMTreeMapIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNMTreeMapIterator* ret = [CNMTreeMapIterator treeMapIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (id)next {
    CNTuple* ret = tuple(_entry.key, _entry.value);
    _prev = _entry;
    _entry = [_entry next];
    return ret;
}

- (void)remove {
    [_map deleteEntry:_entry];
}

- (ODClassType*)type {
    return [CNMTreeMapIterator type];
}

+ (ODClassType*)type {
    return _CNMTreeMapIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNMTreeMapIterator* o = ((CNMTreeMapIterator*)(other));
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


