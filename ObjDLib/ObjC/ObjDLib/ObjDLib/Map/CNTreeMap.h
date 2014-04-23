#import "objdcore.h"
#import "CNMap.h"
#import "CNCollection.h"
#import "ODObject.h"
@class ODClassType;
@class CNMArray;
@class CNDispatchQueue;
@class CNChain;

@class CNTreeMap;
@class CNImTreeMap;
@class CNTreeMapBuilder;
@class CNMTreeMap;
@class CNTreeMapEntry;
@class CNTreeMapKeySet_impl;
@class CNImTreeMapKeySet;
@class CNTreeMapKeyIterator;
@class CNMTreeMapKeySet;
@class CNMTreeMapKeyIterator;
@class CNTreeMapValues;
@class CNTreeMapValuesIterator;
@class CNTreeMapIterator;
@class CNMTreeMapIterator;
@protocol CNTreeMapKeySet;

@interface CNTreeMap : CNImMap_impl {
@protected
    NSInteger(^_comparator)(id, id);
    CNTreeMapValues* _values;
}
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);
@property (nonatomic, readonly) CNTreeMapValues* values;

+ (instancetype)treeMapWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
- (id)applyKey:(id)key;
- (id)optKey:(id)key;
- (CNTreeMapEntry*)root;
- (BOOL)isEmpty;
- (CNTreeMapEntry*)entryForKey:(id)key;
- (id<CNTreeMapKeySet>)keys;
- (id<CNIterator>)iterator;
- (CNTreeMapIterator*)iteratorHigherThanKey:(id)key;
- (CNTreeMapEntry*)firstEntry;
- (id)firstKey;
- (id)lastKey;
- (id)lowerKeyThanKey:(id)key;
- (id)higherKeyThanKey:(id)key;
+ (NSInteger)BLACK;
+ (NSInteger)RED;
+ (ODClassType*)type;
@end


@interface CNImTreeMap : CNTreeMap {
@protected
    CNTreeMapEntry* _root;
    NSUInteger _count;
    id<CNTreeMapKeySet> _keys;
}
@property (nonatomic, readonly) CNTreeMapEntry* root;
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) id<CNTreeMapKeySet> keys;

+ (instancetype)imTreeMapWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count;
- (ODClassType*)type;
- (BOOL)isEmpty;
- (CNMTreeMap*)mCopy;
+ (ODClassType*)type;
@end


@interface CNTreeMapBuilder : CNBuilder_impl {
@protected
    NSInteger(^_comparator)(id, id);
    CNMTreeMap* _map;
}
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (instancetype)treeMapBuilderWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
+ (CNTreeMapBuilder*)apply;
- (void)appendItem:(CNTuple*)item;
- (CNTreeMap*)build;
+ (ODClassType*)type;
@end


@interface CNMTreeMap : CNTreeMap<CNMMap> {
@protected
    CNTreeMapEntry* __root;
    NSUInteger __size;
    CNMTreeMapKeySet* _keys;
}
@property (nonatomic, readonly) CNMTreeMapKeySet* keys;

+ (instancetype)treeMapWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
+ (CNMTreeMap*)apply;
- (CNImTreeMap*)imCopy;
- (CNImTreeMap*)im;
- (void)assignImMap:(id<CNImMap>)imMap;
- (CNTreeMapEntry*)root;
- (NSUInteger)count;
- (void)clear;
- (id<CNMIterator>)mutableIterator;
- (void)setKey:(id)key value:(id)value;
- (id)removeForKey:(id)key;
- (CNTuple*)pollFirst;
+ (ODClassType*)type;
@end


@interface CNTreeMapEntry : NSObject {
@protected
    id _key;
    id _value;
    __weak CNTreeMapEntry* _parent;
    CNTreeMapEntry* _left;
    CNTreeMapEntry* _right;
    NSInteger _color;
}
@property (nonatomic, retain) id key;
@property (nonatomic, retain) id value;
@property (nonatomic, weak) CNTreeMapEntry* parent;
@property (nonatomic) CNTreeMapEntry* left;
@property (nonatomic) CNTreeMapEntry* right;
@property (nonatomic) NSInteger color;

+ (instancetype)treeMapEntryWithKey:(id)key value:(id)value parent:(CNTreeMapEntry*)parent;
- (instancetype)initWithKey:(id)key value:(id)value parent:(CNTreeMapEntry*)parent;
- (ODClassType*)type;
- (CNTreeMapEntry*)next;
- (CNTreeMapEntry*)copyParent:(CNTreeMapEntry*)parent;
+ (ODClassType*)type;
@end


@protocol CNTreeMapKeySet<CNImIterable>
- (id<CNIterator>)iteratorHigherThanKey:(id)key;
@end


@interface CNTreeMapKeySet_impl : CNImIterable_impl<CNTreeMapKeySet>
@end


@interface CNImTreeMapKeySet : CNTreeMapKeySet_impl {
@protected
    __weak CNTreeMap* _map;
}
@property (nonatomic, readonly, weak) CNTreeMap* map;

+ (instancetype)imTreeMapKeySetWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNIterator>)iteratorHigherThanKey:(id)key;
+ (ODClassType*)type;
@end


@interface CNTreeMapKeyIterator : CNIterator_impl {
@protected
    CNTreeMap* _map;
    CNTreeMapEntry* _entry;
}
@property (nonatomic, readonly) CNTreeMap* map;
@property (nonatomic) CNTreeMapEntry* entry;

+ (instancetype)treeMapKeyIteratorWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (CNTreeMapKeyIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNMTreeMapKeySet : CNTreeMapKeySet_impl {
@protected
    __weak CNMTreeMap* _map;
}
@property (nonatomic, readonly, weak) CNMTreeMap* map;

+ (instancetype)treeMapKeySetWithMap:(CNMTreeMap*)map;
- (instancetype)initWithMap:(CNMTreeMap*)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNMIterator>)mutableIterator;
- (id<CNIterator>)iteratorHigherThanKey:(id)key;
+ (ODClassType*)type;
@end


@interface CNMTreeMapKeyIterator : CNMIterator_impl {
@protected
    CNMTreeMap* _map;
    CNTreeMapEntry* _prev;
    CNTreeMapEntry* _entry;
}
@property (nonatomic, readonly) CNMTreeMap* map;
@property (nonatomic) CNTreeMapEntry* entry;

+ (instancetype)treeMapKeyIteratorWithMap:(CNMTreeMap*)map;
- (instancetype)initWithMap:(CNMTreeMap*)map;
- (ODClassType*)type;
+ (CNMTreeMapKeyIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
- (void)remove;
- (void)setValue:(id)value;
+ (ODClassType*)type;
@end


@interface CNTreeMapValues : CNImIterable_impl {
@protected
    __weak CNTreeMap* _map;
}
@property (nonatomic, readonly, weak) CNTreeMap* map;

+ (instancetype)treeMapValuesWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
+ (ODClassType*)type;
@end


@interface CNTreeMapValuesIterator : CNIterator_impl {
@protected
    CNTreeMapEntry* _entry;
}
@property (nonatomic) CNTreeMapEntry* entry;

+ (instancetype)treeMapValuesIteratorWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (CNTreeMapValuesIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNTreeMapIterator : CNIterator_impl {
@protected
    CNTreeMap* _map;
    CNTreeMapEntry* _entry;
}
@property (nonatomic, readonly) CNTreeMap* map;
@property (nonatomic) CNTreeMapEntry* entry;

+ (instancetype)treeMapIteratorWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (CNTreeMapIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (CNTuple*)next;
+ (ODClassType*)type;
@end


@interface CNMTreeMapIterator : CNMIterator_impl {
@protected
    CNMTreeMap* _map;
    CNTreeMapEntry* _prev;
    CNTreeMapEntry* _entry;
}
@property (nonatomic, readonly) CNMTreeMap* map;
@property (nonatomic) CNTreeMapEntry* entry;

+ (instancetype)treeMapIteratorWithMap:(CNMTreeMap*)map;
- (instancetype)initWithMap:(CNMTreeMap*)map;
- (ODClassType*)type;
+ (CNMTreeMapIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (CNTuple*)next;
- (void)remove;
- (void)setValue:(CNTuple*)value;
+ (ODClassType*)type;
@end


