#import "objd.h"
#import "CNTreeMap.h"

#import "CNType.h"
#import "CNTuple.h"
#import "CNPlat.h"
#import "CNDispatchQueue.h"
#import "CNChain.h"
@implementation CNTreeMap
static NSInteger _CNTreeMap_BLACK = 0;
static NSInteger _CNTreeMap_RED = 1;
static CNClassType* _CNTreeMap_type;
@synthesize comparator = _comparator;
@synthesize values = _values;

+ (instancetype)treeMapWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNTreeMap alloc] initWithComparator:comparator];
}

- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) {
        _comparator = [comparator copy];
        _values = [CNTreeMapValues treeMapValuesWithMap:self];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMap class]) _CNTreeMap_type = [CNClassType classTypeWithCls:[CNTreeMap class]];
}

- (id)applyKey:(id)key {
    return ((CNTreeMapEntry*)([self entryForKey:key])).value;
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
        NSInteger cmp = _comparator(key, ((CNTreeMapEntry*)(p)).key);
        if(cmp < 0) {
            p = ((CNTreeMapEntry*)(p)).left;
        } else {
            if(cmp > 0) p = ((CNTreeMapEntry*)(p)).right;
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
    return [CNTreeMapIterator applyMap:self entry:[self higherEntryThanKey:key]];
}

- (CNTreeMapEntry*)firstEntry {
    CNTreeMapEntry* p = [self root];
    if(p != nil) while(((CNTreeMapEntry*)(p)).left != nil) {
        p = ((CNTreeMapEntry*)(p)).left;
    }
    return p;
}

- (id)firstKey {
    return ((CNTreeMapEntry*)([self firstEntry])).key;
}

- (id)lastKey {
    return ((CNTreeMapEntry*)([self lastEntry])).key;
}

- (id)lowerKeyThanKey:(id)key {
    return ((CNTreeMapEntry*)([self lowerEntryThanKey:key])).key;
}

- (id)higherKeyThanKey:(id)key {
    return ((CNTreeMapEntry*)([self higherEntryThanKey:key])).key;
}

- (CNTreeMapEntry*)lowerEntryThanKey:(id)key {
    CNTreeMapEntry* p = [self root];
    while(p != nil) {
        NSInteger cmp = _comparator(key, ((CNTreeMapEntry*)(p)).key);
        if(cmp > 0) {
            if(((CNTreeMapEntry*)(p)).right != nil) p = ((CNTreeMapEntry*)(p)).right;
            else return p;
        } else {
            if(((CNTreeMapEntry*)(p)).left != nil) {
                p = ((CNTreeMapEntry*)(p)).left;
            } else {
                CNTreeMapEntry* parent = ((CNTreeMapEntry*)(p)).parent;
                CNTreeMapEntry* ch = p;
                while(parent != nil && ({
    CNTreeMapEntry* __tmp_1_1f_0f_2b = ((CNTreeMapEntry*)(parent)).left;
    __tmp_1_1f_0f_2b != nil && [__tmp_1_1f_0f_2b isEqual:ch];
})) {
                    ch = parent;
                    parent = ((CNTreeMapEntry*)(parent)).parent;
                }
                return parent;
            }
        }
    }
    return nil;
}

- (CNTreeMapEntry*)higherEntryThanKey:(id)key {
    CNTreeMapEntry* p = [self root];
    while(p != nil) {
        NSInteger cmp = _comparator(key, ((CNTreeMapEntry*)(p)).key);
        if(cmp < 0) {
            if(((CNTreeMapEntry*)(p)).left != nil) p = ((CNTreeMapEntry*)(p)).left;
            else return p;
        } else {
            if(((CNTreeMapEntry*)(p)).right != nil) {
                p = ((CNTreeMapEntry*)(p)).right;
            } else {
                CNTreeMapEntry* parent = ((CNTreeMapEntry*)(p)).parent;
                CNTreeMapEntry* ch = p;
                while(parent != nil && ({
    CNTreeMapEntry* __tmp_1_1f_0f_2b = ((CNTreeMapEntry*)(parent)).right;
    __tmp_1_1f_0f_2b != nil && [__tmp_1_1f_0f_2b isEqual:ch];
})) {
                    ch = parent;
                    parent = ((CNTreeMapEntry*)(parent)).parent;
                }
                return parent;
            }
        }
    }
    return nil;
}

- (CNTreeMapEntry*)lastEntry {
    CNTreeMapEntry* p = [self root];
    if(p != nil) while(((CNTreeMapEntry*)(p)).right != nil) {
        p = ((CNTreeMapEntry*)(p)).right;
    }
    return p;
}

- (NSString*)description {
    return [NSString stringWithFormat:@")"];
}

- (CNClassType*)type {
    return [CNTreeMap type];
}

+ (NSInteger)BLACK {
    return _CNTreeMap_BLACK;
}

+ (NSInteger)RED {
    return _CNTreeMap_RED;
}

+ (CNClassType*)type {
    return _CNTreeMap_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNImTreeMap
static CNClassType* _CNImTreeMap_type;
@synthesize root = _root;
@synthesize count = _count;
@synthesize keys = _keys;

+ (instancetype)imTreeMapWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count {
    return [[CNImTreeMap alloc] initWithComparator:comparator root:root count:count];
}

- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count {
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
    if(self == [CNImTreeMap class]) _CNImTreeMap_type = [CNClassType classTypeWithCls:[CNImTreeMap class]];
}

- (BOOL)isEmpty {
    return _root == nil;
}

- (CNMTreeMap*)mCopy {
    CNMTreeMap* m = [CNMTreeMap treeMapWithComparator:self.comparator];
    [m assignImMap:self];
    return m;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"ImTreeMap(%@, %lu)", _root, (unsigned long)_count];
}

- (CNClassType*)type {
    return [CNImTreeMap type];
}

+ (CNClassType*)type {
    return _CNImTreeMap_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapBuilder
static CNClassType* _CNTreeMapBuilder_type;
@synthesize comparator = _comparator;

+ (instancetype)treeMapBuilderWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNTreeMapBuilder alloc] initWithComparator:comparator];
}

- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator {
    self = [super init];
    if(self) {
        _comparator = [comparator copy];
        _map = [CNMTreeMap treeMapWithComparator:comparator];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapBuilder class]) _CNTreeMapBuilder_type = [CNClassType classTypeWithCls:[CNTreeMapBuilder class]];
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

- (NSString*)description {
    return [NSString stringWithFormat:@")"];
}

- (CNClassType*)type {
    return [CNTreeMapBuilder type];
}

+ (CNClassType*)type {
    return _CNTreeMapBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMTreeMap
static CNClassType* _CNMTreeMap_type;
@synthesize keys = _keys;

+ (instancetype)treeMapWithComparator:(NSInteger(^)(id, id))comparator {
    return [[CNMTreeMap alloc] initWithComparator:comparator];
}

- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator {
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
    if(self == [CNMTreeMap class]) _CNMTreeMap_type = [CNClassType classTypeWithCls:[CNMTreeMap class]];
}

+ (CNMTreeMap*)apply {
    return [CNMTreeMap treeMapWithComparator:^NSInteger(id a, id b) {
        return [a compareTo:b];
    }];
}

- (CNImTreeMap*)imCopy {
    return [CNImTreeMap imTreeMapWithComparator:self.comparator root:[((CNTreeMapEntry*)(__root)) copyParent:nil] count:__size];
}

- (CNImTreeMap*)im {
    return [CNImTreeMap imTreeMapWithComparator:self.comparator root:__root count:__size];
}

- (void)assignImMap:(id<CNImMap>)imMap {
    if([imMap isKindOfClass:[CNImTreeMap class]]) {
        CNImTreeMap* m = ((CNImTreeMap*)(imMap));
        __root = [((CNTreeMapEntry*)(m.root)) copyParent:nil];
        __size = m.count;
    } else {
        [self clear];
        {
            id<CNIterator> __il__0f_1i = [imMap iterator];
            while([__il__0f_1i hasNext]) {
                CNTuple* _ = [__il__0f_1i next];
                [self appendItem:_];
            }
        }
    }
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

- (id<CNMIterator>)mutableIterator {
    return [CNMTreeMapIterator applyMap:self entry:[self firstEntry]];
}

- (void)setKey:(id)key value:(id)value {
    NSInteger(^__comparator)(id, id) = self.comparator;
    CNTreeMapEntry* t = __root;
    if(t == nil) {
        __root = [CNTreeMapEntry treeMapEntryWithKey:key value:value parent:nil];
        __size = 1;
    } else {
        NSInteger cmp = 0;
        CNTreeMapEntry* parent = nil;
        do {
            parent = ((CNTreeMapEntry*)(t));
            cmp = __comparator(key, ((CNTreeMapEntry*)(t)).key);
            if(cmp < 0) {
                t = ((CNTreeMapEntry*)(t)).left;
            } else {
                if(cmp > 0) {
                    t = ((CNTreeMapEntry*)(t)).right;
                } else {
                    ((CNTreeMapEntry*)(t)).value = value;
                    return ;
                }
            }
        } while(t != nil);
        CNTreeMapEntry* e = [CNTreeMapEntry treeMapEntryWithKey:key value:value parent:parent];
        if(cmp < 0) ((CNTreeMapEntry*)(nonnil(parent))).left = e;
        else ((CNTreeMapEntry*)(nonnil(parent))).right = e;
        [self fixAfterInsertionEntry:e];
        __size++;
    }
}

- (id)removeKey:(id)key {
    CNTreeMapEntry* _ = [self entryForKey:key];
    if(_ != nil) return [self deleteEntry:_];
    else return nil;
}

- (BOOL)removeItem:(CNTuple*)item {
    return [self removeKey:((CNTuple*)(item)).a] != nil;
}

- (id)deleteEntry:(CNTreeMapEntry*)entry {
    CNTreeMapEntry* p = entry;
    __size--;
    if(p.left != nil && p.right != nil) {
        CNTreeMapEntry* s = ((CNTreeMapEntry*)(nonnil([p next])));
        p.key = s.key;
        p.value = s.value;
        p = s;
    }
    CNTreeMapEntry* replacement = ((p.left != nil) ? p.left : p.right);
    if(replacement != nil) {
        ((CNTreeMapEntry*)(replacement)).parent = p.parent;
        if(p.parent == nil) {
            __root = ((CNTreeMapEntry*)(replacement));
        } else {
            if(({
                CNTreeMapEntry* __tmp_4t_1fc = ((CNTreeMapEntry*)(nonnil(p.parent))).left;
                __tmp_4t_1fc != nil && [__tmp_4t_1fc isEqual:p];
            })) ((CNTreeMapEntry*)(nonnil(p.parent))).left = ((CNTreeMapEntry*)(replacement));
            else ((CNTreeMapEntry*)(nonnil(p.parent))).right = ((CNTreeMapEntry*)(replacement));
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
            CNTreeMapEntry* g = p.parent;
            if(g != nil) {
                if(({
                    CNTreeMapEntry* __tmp_4ff_2t_0c = ((CNTreeMapEntry*)(g)).left;
                    __tmp_4ff_2t_0c != nil && [__tmp_4ff_2t_0c isEqual:p];
                })) {
                    ((CNTreeMapEntry*)(g)).left = nil;
                } else {
                    if(({
                        CNTreeMapEntry* __tmp_4ff_2t_0fc = ((CNTreeMapEntry*)(g)).right;
                        __tmp_4ff_2t_0fc != nil && [__tmp_4ff_2t_0fc isEqual:p];
                    })) ((CNTreeMapEntry*)(g)).right = nil;
                }
                p.parent = nil;
            }
        }
    }
    return entry.value;
}

- (void)fixAfterInsertionEntry:(CNTreeMapEntry*)entry {
    entry.color = [CNMTreeMap RED];
    CNTreeMapEntry* x = entry;
    while(x != nil && (__root == nil || !([__root isEqual:x])) && ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(x)).parent))).color == [CNMTreeMap RED]) {
        CNTreeMapEntry* p = ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(x)).parent)));
        if(({
            CNTreeMapEntry* __tmp_2_1c = ((CNTreeMapEntry*)(p.parent)).left;
            __tmp_2_1c != nil && [__tmp_2_1c isEqual:p];
        })) {
            CNTreeMapEntry* y = ((CNTreeMapEntry*)(p.parent)).right;
            if(y != nil && ((CNTreeMapEntry*)(y)).color == [CNMTreeMap RED]) {
                p.color = [CNMTreeMap BLACK];
                ((CNTreeMapEntry*)(y)).color = [CNMTreeMap BLACK];
                {
                    CNTreeMapEntry* __tmp_2_1t_1t_2 = p.parent;
                    if(__tmp_2_1t_1t_2 != nil) ((CNTreeMapEntry*)(__tmp_2_1t_1t_2)).color = [CNMTreeMap RED];
                }
                x = p.parent;
            } else {
                if(({
                    CNTreeMapEntry* __tmp_2_1t_1f_0c = p.right;
                    __tmp_2_1t_1f_0c != nil && [__tmp_2_1t_1f_0c isEqual:x];
                })) {
                    x = p;
                    [self rotateLeftP:((CNTreeMapEntry*)(x))];
                }
                CNTreeMapEntry* pp = ((CNTreeMapEntry*)(x)).parent;
                if(pp != nil) {
                    ((CNTreeMapEntry*)(pp)).color = [CNMTreeMap BLACK];
                    {
                        CNTreeMapEntry* __tmp_2_1t_1f_2t_1 = ((CNTreeMapEntry*)(pp)).parent;
                        if(__tmp_2_1t_1f_2t_1 != nil) ((CNTreeMapEntry*)(__tmp_2_1t_1f_2t_1)).color = [CNMTreeMap RED];
                    }
                    [self rotateRightP:((CNTreeMapEntry*)(pp)).parent];
                }
            }
        } else {
            CNTreeMapEntry* y = ((CNTreeMapEntry*)(p.parent)).left;
            if(y != nil && ((CNTreeMapEntry*)(y)).color == [CNMTreeMap RED]) {
                p.color = [CNMTreeMap BLACK];
                ((CNTreeMapEntry*)(y)).color = [CNMTreeMap BLACK];
                {
                    CNTreeMapEntry* __tmp_2_1f_1t_2 = p.parent;
                    if(__tmp_2_1f_1t_2 != nil) ((CNTreeMapEntry*)(__tmp_2_1f_1t_2)).color = [CNMTreeMap RED];
                }
                x = p.parent;
            } else {
                if(({
                    CNTreeMapEntry* __tmp_2_1f_1f_0c = p.left;
                    __tmp_2_1f_1f_0c != nil && [__tmp_2_1f_1f_0c isEqual:x];
                })) {
                    x = p;
                    [self rotateRightP:((CNTreeMapEntry*)(x))];
                }
                CNTreeMapEntry* pp = ((CNTreeMapEntry*)(x)).parent;
                if(pp != nil) {
                    ((CNTreeMapEntry*)(pp)).color = [CNMTreeMap BLACK];
                    {
                        CNTreeMapEntry* __tmp_2_1f_1f_2t_1 = ((CNTreeMapEntry*)(pp)).parent;
                        if(__tmp_2_1f_1f_2t_1 != nil) ((CNTreeMapEntry*)(__tmp_2_1f_1f_2t_1)).color = [CNMTreeMap RED];
                    }
                    [self rotateLeftP:((CNTreeMapEntry*)(pp)).parent];
                }
            }
        }
    }
    if(__root != nil) ((CNTreeMapEntry*)(__root)).color = [CNMTreeMap BLACK];
}

- (void)fixAfterDeletionEntry:(CNTreeMapEntry*)entry {
    CNTreeMapEntry* x = entry;
    while(x != nil && (__root == nil || !([__root isEqual:x])) && ((CNTreeMapEntry*)(x)).color == [CNMTreeMap BLACK]) {
        if(({
            CNTreeMapEntry* __tmp_1_0c = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).left;
            __tmp_1_0c != nil && [__tmp_1_0c isEqual:x];
        })) {
            CNTreeMapEntry* sib = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).right;
            if(sib != nil && ((CNTreeMapEntry*)(sib)).color == [CNMTreeMap RED]) {
                ((CNTreeMapEntry*)(sib)).color = [CNMTreeMap BLACK];
                {
                    CNTreeMapEntry* __tmp_1_0t_1t_1 = ((CNTreeMapEntry*)(x)).parent;
                    if(__tmp_1_0t_1t_1 != nil) ((CNTreeMapEntry*)(__tmp_1_0t_1t_1)).color = [CNMTreeMap RED];
                }
                [self rotateLeftP:((CNTreeMapEntry*)(x)).parent];
                sib = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).right;
            }
            if(({
                CNTreeMapEntry* __tmp_1_0t_2caa = ((CNTreeMapEntry*)(sib)).left;
                ((__tmp_1_0t_2caa != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(sib)).left)).color : [CNMTreeMap BLACK]);
            }) == [CNMTreeMap BLACK] && ({
                CNTreeMapEntry* __tmp_1_0t_2cba = ((CNTreeMapEntry*)(sib)).right;
                ((__tmp_1_0t_2cba != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(sib)).right)).color : [CNMTreeMap BLACK]);
            }) == [CNMTreeMap BLACK]) {
                if(sib != nil) ((CNTreeMapEntry*)(sib)).color = [CNMTreeMap RED];
                x = ((CNTreeMapEntry*)(x)).parent;
            } else {
                if(({
                    CNTreeMapEntry* __tmp_1_0t_2f_0ca = ((CNTreeMapEntry*)(sib)).right;
                    ((__tmp_1_0t_2f_0ca != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(sib)).right)).color : [CNMTreeMap BLACK]);
                }) == [CNMTreeMap BLACK]) {
                    {
                        CNTreeMapEntry* __tmp_1_0t_2f_0t_0 = ((CNTreeMapEntry*)(sib)).left;
                        if(__tmp_1_0t_2f_0t_0 != nil) ((CNTreeMapEntry*)(__tmp_1_0t_2f_0t_0)).color = [CNMTreeMap BLACK];
                    }
                    if(sib != nil) ((CNTreeMapEntry*)(sib)).color = [CNMTreeMap RED];
                    [self rotateRightP:sib];
                    sib = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).right;
                }
                if(sib != nil) ((CNTreeMapEntry*)(sib)).color = ({
                    CNTreeMapEntry* __tmp_1_0t_2f_1 = ((CNTreeMapEntry*)(x)).parent;
                    ((__tmp_1_0t_2f_1 != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).color : [CNMTreeMap BLACK]);
                });
                {
                    CNTreeMapEntry* __tmp_1_0t_2f_2 = ((CNTreeMapEntry*)(x)).parent;
                    if(__tmp_1_0t_2f_2 != nil) ((CNTreeMapEntry*)(__tmp_1_0t_2f_2)).color = [CNMTreeMap BLACK];
                }
                {
                    CNTreeMapEntry* __tmp_1_0t_2f_3 = ((CNTreeMapEntry*)(sib)).right;
                    if(__tmp_1_0t_2f_3 != nil) ((CNTreeMapEntry*)(__tmp_1_0t_2f_3)).color = [CNMTreeMap BLACK];
                }
                [self rotateLeftP:((CNTreeMapEntry*)(x)).parent];
                x = __root;
            }
        } else {
            CNTreeMapEntry* sib = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).left;
            if(sib != nil && ((CNTreeMapEntry*)(sib)).color == [CNMTreeMap RED]) {
                ((CNTreeMapEntry*)(sib)).color = [CNMTreeMap BLACK];
                {
                    CNTreeMapEntry* __tmp_1_0f_1t_1 = ((CNTreeMapEntry*)(x)).parent;
                    if(__tmp_1_0f_1t_1 != nil) ((CNTreeMapEntry*)(__tmp_1_0f_1t_1)).color = [CNMTreeMap RED];
                }
                [self rotateRightP:((CNTreeMapEntry*)(x)).parent];
                sib = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).left;
            }
            if(({
                CNTreeMapEntry* __tmp_1_0f_2caa = ((CNTreeMapEntry*)(sib)).right;
                ((__tmp_1_0f_2caa != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(sib)).right)).color : [CNMTreeMap BLACK]);
            }) == [CNMTreeMap BLACK] && ({
                CNTreeMapEntry* __tmp_1_0f_2cba = ((CNTreeMapEntry*)(sib)).left;
                ((__tmp_1_0f_2cba != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(sib)).left)).color : [CNMTreeMap BLACK]);
            }) == [CNMTreeMap BLACK]) {
                if(sib != nil) ((CNTreeMapEntry*)(sib)).color = [CNMTreeMap RED];
                x = ((CNTreeMapEntry*)(x)).parent;
            } else {
                if(({
                    CNTreeMapEntry* __tmp_1_0f_2f_0ca = ((CNTreeMapEntry*)(sib)).left;
                    ((__tmp_1_0f_2f_0ca != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(sib)).left)).color : [CNMTreeMap BLACK]);
                }) == [CNMTreeMap BLACK]) {
                    {
                        CNTreeMapEntry* __tmp_1_0f_2f_0t_0 = ((CNTreeMapEntry*)(sib)).right;
                        if(__tmp_1_0f_2f_0t_0 != nil) ((CNTreeMapEntry*)(__tmp_1_0f_2f_0t_0)).color = [CNMTreeMap BLACK];
                    }
                    if(sib != nil) ((CNTreeMapEntry*)(sib)).color = [CNMTreeMap RED];
                    [self rotateLeftP:sib];
                    sib = ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).left;
                }
                if(sib != nil) ((CNTreeMapEntry*)(sib)).color = ({
                    CNTreeMapEntry* __tmp_1_0f_2f_1 = ((CNTreeMapEntry*)(x)).parent;
                    ((__tmp_1_0f_2f_1 != nil) ? ((CNTreeMapEntry*)(((CNTreeMapEntry*)(x)).parent)).color : [CNMTreeMap BLACK]);
                });
                {
                    CNTreeMapEntry* __tmp_1_0f_2f_2 = ((CNTreeMapEntry*)(x)).parent;
                    if(__tmp_1_0f_2f_2 != nil) ((CNTreeMapEntry*)(__tmp_1_0f_2f_2)).color = [CNMTreeMap BLACK];
                }
                {
                    CNTreeMapEntry* __tmp_1_0f_2f_3 = ((CNTreeMapEntry*)(sib)).left;
                    if(__tmp_1_0f_2f_3 != nil) ((CNTreeMapEntry*)(__tmp_1_0f_2f_3)).color = [CNMTreeMap BLACK];
                }
                [self rotateRightP:((CNTreeMapEntry*)(x)).parent];
                x = __root;
            }
        }
    }
    if(x != nil) ((CNTreeMapEntry*)(x)).color = [CNMTreeMap BLACK];
}

- (void)rotateLeftP:(CNTreeMapEntry*)p {
    if(p != nil) {
        CNTreeMapEntry* r = ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).right)));
        ((CNTreeMapEntry*)(p)).right = r.left;
        {
            CNTreeMapEntry* __tmp_0t_2 = r.left;
            if(__tmp_0t_2 != nil) ((CNTreeMapEntry*)(__tmp_0t_2)).parent = ((CNTreeMapEntry*)(p));
        }
        r.parent = ((CNTreeMapEntry*)(p)).parent;
        if(((CNTreeMapEntry*)(p)).parent == nil) {
            __root = r;
        } else {
            if(({
                CNTreeMapEntry* __tmp_0t_4fc = ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).parent))).left;
                __tmp_0t_4fc != nil && [__tmp_0t_4fc isEqual:p];
            })) ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).parent))).left = r;
            else ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).parent))).right = r;
        }
        r.left = ((CNTreeMapEntry*)(p));
        ((CNTreeMapEntry*)(p)).parent = r;
    }
}

- (void)rotateRightP:(CNTreeMapEntry*)p {
    if(p != nil) {
        CNTreeMapEntry* l = ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).left)));
        ((CNTreeMapEntry*)(p)).left = l.right;
        {
            CNTreeMapEntry* __tmp_0t_2 = l.right;
            if(__tmp_0t_2 != nil) ((CNTreeMapEntry*)(__tmp_0t_2)).parent = ((CNTreeMapEntry*)(p));
        }
        l.parent = ((CNTreeMapEntry*)(p)).parent;
        if(((CNTreeMapEntry*)(p)).parent == nil) {
            __root = l;
        } else {
            if(({
                CNTreeMapEntry* __tmp_0t_4fc = ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).parent))).right;
                __tmp_0t_4fc != nil && [__tmp_0t_4fc isEqual:p];
            })) ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).parent))).right = l;
            else ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).parent))).left = l;
        }
        l.right = ((CNTreeMapEntry*)(p));
        ((CNTreeMapEntry*)(p)).parent = l;
    }
}

- (CNTuple*)pollFirst {
    CNTreeMapEntry* entry = [self firstEntry];
    if(entry != nil) {
        [self deleteEntry:entry];
        return tuple(((CNTreeMapEntry*)(entry)).key, ((CNTreeMapEntry*)(entry)).value);
    } else {
        return nil;
    }
}

- (NSString*)description {
    return @"MTreeMap";
}

- (id)applyKey:(id)key orUpdateWith:(id(^)())orUpdateWith {
    id __tmp = [self applyKey:key];
    if(__tmp != nil) {
        return __tmp;
    } else {
        id init = orUpdateWith();
        [self setKey:key value:init];
        return init;
    }
}

- (id)modifyKey:(id)key by:(id(^)(id))by {
    id newObject = by([self applyKey:key]);
    if(newObject == nil) [self removeKey:key];
    else [self setKey:key value:newObject];
    return newObject;
}

- (void)appendItem:(CNTuple*)item {
    [self setKey:((CNTuple*)(item)).a value:((CNTuple*)(item)).b];
}

- (void)mutableFilterBy:(BOOL(^)(CNTuple*))by {
    id<CNMIterator> i = [self mutableIterator];
    while([i hasNext]) {
        if(by([i next])) [i remove];
    }
}

- (CNClassType*)type {
    return [CNMTreeMap type];
}

+ (CNClassType*)type {
    return _CNMTreeMap_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapEntry
static CNClassType* _CNTreeMapEntry_type;
@synthesize key = _key;
@synthesize value = _value;
@synthesize parent = _parent;
@synthesize left = _left;
@synthesize right = _right;
@synthesize color = _color;

+ (instancetype)treeMapEntryWithKey:(id)key value:(id)value parent:(CNTreeMapEntry*)parent {
    return [[CNTreeMapEntry alloc] initWithKey:key value:value parent:parent];
}

- (instancetype)initWithKey:(id)key value:(id)value parent:(CNTreeMapEntry*)parent {
    self = [super init];
    if(self) {
        _key = key;
        _value = value;
        _parent = parent;
        _left = nil;
        _right = nil;
        _color = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapEntry class]) _CNTreeMapEntry_type = [CNClassType classTypeWithCls:[CNTreeMapEntry class]];
}

- (CNTreeMapEntry*)next {
    if(_right != nil) {
        CNTreeMapEntry* p = _right;
        while(((CNTreeMapEntry*)(p)).left != nil) {
            p = ((CNTreeMapEntry*)(nonnil(((CNTreeMapEntry*)(p)).left)));
        }
        return ((CNTreeMapEntry*)(p));
    } else {
        CNTreeMapEntry* p = _parent;
        CNTreeMapEntry* ch = self;
        while(p != nil && ch == ((CNTreeMapEntry*)(p)).right) {
            ch = p;
            p = ((CNTreeMapEntry*)(p)).parent;
        }
        return p;
    }
}

- (CNTreeMapEntry*)copyParent:(CNTreeMapEntry*)parent {
    CNTreeMapEntry* c = [CNTreeMapEntry treeMapEntryWithKey:_key value:_value parent:parent];
    c.left = [((CNTreeMapEntry*)(_left)) copyParent:c];
    c.right = [((CNTreeMapEntry*)(_right)) copyParent:c];
    c.color = _color;
    return c;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"TreeMapEntry(%@, %@, %@)", _key, _value, _parent];
}

- (CNClassType*)type {
    return [CNTreeMapEntry type];
}

+ (CNClassType*)type {
    return _CNTreeMapEntry_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapKeySet_impl

+ (instancetype)treeMapKeySet_impl {
    return [[CNTreeMapKeySet_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (id<CNIterator>)iteratorHigherThanKey:(id)key {
    @throw @"Method iteratorHigherThan is abstract";
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNImTreeMapKeySet
static CNClassType* _CNImTreeMapKeySet_type;
@synthesize map = _map;

+ (instancetype)imTreeMapKeySetWithMap:(CNTreeMap*)map {
    return [[CNImTreeMapKeySet alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImTreeMapKeySet class]) _CNImTreeMapKeySet_type = [CNClassType classTypeWithCls:[CNImTreeMapKeySet class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [CNTreeMapKeyIterator applyMap:_map entry:[_map firstEntry]];
}

- (id<CNIterator>)iteratorHigherThanKey:(id)key {
    return [CNTreeMapKeyIterator applyMap:_map entry:[_map higherEntryThanKey:key]];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"ImTreeMapKeySet(%@)", _map];
}

- (CNClassType*)type {
    return [CNImTreeMapKeySet type];
}

+ (CNClassType*)type {
    return _CNImTreeMapKeySet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapKeyIterator
static CNClassType* _CNTreeMapKeyIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (instancetype)treeMapKeyIteratorWithMap:(CNTreeMap*)map {
    return [[CNTreeMapKeyIterator alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapKeyIterator class]) _CNTreeMapKeyIterator_type = [CNClassType classTypeWithCls:[CNTreeMapKeyIterator class]];
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
    CNTreeMapEntry* e = ((CNTreeMapEntry*)(nonnil(_entry)));
    id ret = e.key;
    _entry = [e next];
    return ret;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"TreeMapKeyIterator(%@)", _map];
}

- (CNClassType*)type {
    return [CNTreeMapKeyIterator type];
}

+ (CNClassType*)type {
    return _CNTreeMapKeyIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMTreeMapKeySet
static CNClassType* _CNMTreeMapKeySet_type;
@synthesize map = _map;

+ (instancetype)treeMapKeySetWithMap:(CNMTreeMap*)map {
    return [[CNMTreeMapKeySet alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNMTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMTreeMapKeySet class]) _CNMTreeMapKeySet_type = [CNClassType classTypeWithCls:[CNMTreeMapKeySet class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [CNTreeMapKeyIterator applyMap:_map entry:[_map firstEntry]];
}

- (id<CNMIterator>)mutableIterator {
    return [CNMTreeMapKeyIterator applyMap:_map entry:[_map firstEntry]];
}

- (id<CNIterator>)iteratorHigherThanKey:(id)key {
    return [CNMTreeMapKeyIterator applyMap:_map entry:[_map higherEntryThanKey:key]];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"MTreeMapKeySet(%@)", _map];
}

- (CNClassType*)type {
    return [CNMTreeMapKeySet type];
}

+ (CNClassType*)type {
    return _CNMTreeMapKeySet_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMTreeMapKeyIterator
static CNClassType* _CNMTreeMapKeyIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (instancetype)treeMapKeyIteratorWithMap:(CNMTreeMap*)map {
    return [[CNMTreeMapKeyIterator alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNMTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMTreeMapKeyIterator class]) _CNMTreeMapKeyIterator_type = [CNClassType classTypeWithCls:[CNMTreeMapKeyIterator class]];
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
    CNTreeMapEntry* e = ((CNTreeMapEntry*)(nonnil(_entry)));
    id ret = e.key;
    _prev = e;
    _entry = [e next];
    return ret;
}

- (void)remove {
    CNTreeMapEntry* _ = _prev;
    if(_ != nil) [_map deleteEntry:_];
}

- (void)setValue:(id)value {
    CNTreeMapEntry* p = _prev;
    if(p != nil) {
        if(!([((CNTreeMapEntry*)(p)).key isEqual:value])) {
            [_map deleteEntry:p];
            [_map setKey:value value:((CNTreeMapEntry*)(p)).value];
        }
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"MTreeMapKeyIterator(%@)", _map];
}

- (CNClassType*)type {
    return [CNMTreeMapKeyIterator type];
}

+ (CNClassType*)type {
    return _CNMTreeMapKeyIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapValues
static CNClassType* _CNTreeMapValues_type;
@synthesize map = _map;

+ (instancetype)treeMapValuesWithMap:(CNTreeMap*)map {
    return [[CNTreeMapValues alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapValues class]) _CNTreeMapValues_type = [CNClassType classTypeWithCls:[CNTreeMapValues class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [CNTreeMapValuesIterator applyMap:_map entry:[_map firstEntry]];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"TreeMapValues(%@)", _map];
}

- (CNClassType*)type {
    return [CNTreeMapValues type];
}

+ (CNClassType*)type {
    return _CNTreeMapValues_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapValuesIterator
static CNClassType* _CNTreeMapValuesIterator_type;
@synthesize entry = _entry;

+ (instancetype)treeMapValuesIteratorWithMap:(CNTreeMap*)map {
    return [[CNTreeMapValuesIterator alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNTreeMap*)map {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapValuesIterator class]) _CNTreeMapValuesIterator_type = [CNClassType classTypeWithCls:[CNTreeMapValuesIterator class]];
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
    id ret = ((CNTreeMapEntry*)(nonnil(_entry))).value;
    _entry = [((CNTreeMapEntry*)(nonnil(_entry))) next];
    return ret;
}

- (NSString*)description {
    return @"TreeMapValuesIterator";
}

- (CNClassType*)type {
    return [CNTreeMapValuesIterator type];
}

+ (CNClassType*)type {
    return _CNTreeMapValuesIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNTreeMapIterator
static CNClassType* _CNTreeMapIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (instancetype)treeMapIteratorWithMap:(CNTreeMap*)map {
    return [[CNTreeMapIterator alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTreeMapIterator class]) _CNTreeMapIterator_type = [CNClassType classTypeWithCls:[CNTreeMapIterator class]];
}

+ (CNTreeMapIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNTreeMapIterator* ret = [CNTreeMapIterator treeMapIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (CNTuple*)next {
    CNTuple* ret = tuple(((CNTreeMapEntry*)(nonnil(_entry))).key, ((CNTreeMapEntry*)(nonnil(_entry))).value);
    _entry = [((CNTreeMapEntry*)(nonnil(_entry))) next];
    return ret;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"TreeMapIterator(%@)", _map];
}

- (CNClassType*)type {
    return [CNTreeMapIterator type];
}

+ (CNClassType*)type {
    return _CNTreeMapIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMTreeMapIterator
static CNClassType* _CNMTreeMapIterator_type;
@synthesize map = _map;
@synthesize entry = _entry;

+ (instancetype)treeMapIteratorWithMap:(CNMTreeMap*)map {
    return [[CNMTreeMapIterator alloc] initWithMap:map];
}

- (instancetype)initWithMap:(CNMTreeMap*)map {
    self = [super init];
    if(self) _map = map;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMTreeMapIterator class]) _CNMTreeMapIterator_type = [CNClassType classTypeWithCls:[CNMTreeMapIterator class]];
}

+ (CNMTreeMapIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry {
    CNMTreeMapIterator* ret = [CNMTreeMapIterator treeMapIteratorWithMap:map];
    ret.entry = entry;
    return ret;
}

- (BOOL)hasNext {
    return _entry != nil;
}

- (CNTuple*)next {
    CNTreeMapEntry* e = ((CNTreeMapEntry*)(nonnil(_entry)));
    CNTuple* ret = tuple(e.key, e.value);
    _prev = e;
    _entry = [e next];
    return ret;
}

- (void)remove {
    CNTreeMapEntry* _ = _prev;
    if(_ != nil) [_map deleteEntry:_];
}

- (void)setValue:(CNTuple*)value {
    CNTreeMapEntry* p = _prev;
    if(p != nil) {
        if([((CNTreeMapEntry*)(p)).key isEqual:((CNTuple*)(value)).a]) {
            ((CNTreeMapEntry*)(p)).value = ((CNTuple*)(value)).b;
        } else {
            [_map deleteEntry:p];
            [_map setKey:((CNTuple*)(value)).a value:((CNTuple*)(value)).b];
        }
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"MTreeMapIterator(%@)", _map];
}

- (CNClassType*)type {
    return [CNMTreeMapIterator type];
}

+ (CNClassType*)type {
    return _CNMTreeMapIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

