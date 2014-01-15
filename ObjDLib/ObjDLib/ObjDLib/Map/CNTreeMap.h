#import "objdcore.h"
#import "CNMap.h"
#import "CNCollection.h"
#import "ODObject.h"
@class CNSome;
@class ODClassType;
@class CNChain;

@class CNTreeMap;
@class CNImTreeMap;
@class CNTreeMapBuilder;
@class CNMTreeMap;
@class CNTreeMapEntry;
@class CNImTreeMapKeySet;
@class CNTreeMapKeyIterator;
@class CNMTreeMapKeySet;
@class CNMTreeMapKeyIterator;
@class CNTreeMapValues;
@class CNTreeMapValuesIterator;
@class CNTreeMapIterator;
@class CNMTreeMapIterator;
@protocol CNTreeMapKeySet;

@interface CNTreeMap : NSObject<CNMap>
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);
@property (nonatomic, readonly) CNTreeMapValues* values;

+ (id)treeMapWithComparator:(NSInteger(^)(id, id))comparator;
- (id)initWithComparator:(NSInteger(^)(id, id))comparator;
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


@interface CNImTreeMap : CNTreeMap
@property (nonatomic, readonly) CNTreeMapEntry* root;
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) id<CNTreeMapKeySet> keys;

+ (id)imTreeMapWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count;
- (id)initWithComparator:(NSInteger(^)(id, id))comparator root:(CNTreeMapEntry*)root count:(NSUInteger)count;
- (ODClassType*)type;
- (BOOL)isEmpty;
+ (ODClassType*)type;
@end


@interface CNTreeMapBuilder : NSObject<CNBuilder>
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (id)treeMapBuilderWithComparator:(NSInteger(^)(id, id))comparator;
- (id)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
+ (CNTreeMapBuilder*)apply;
- (void)appendItem:(CNTuple*)item;
- (CNTreeMap*)build;
+ (ODClassType*)type;
@end


@interface CNMTreeMap : CNTreeMap<CNMutableMap>
@property (nonatomic, readonly) CNMTreeMapKeySet* keys;

+ (id)treeMapWithComparator:(NSInteger(^)(id, id))comparator;
- (id)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
+ (CNMTreeMap*)apply;
- (CNTreeMapEntry*)root;
- (NSUInteger)count;
- (void)clear;
- (id<CNMutableIterator>)mutableIterator;
- (void)setKey:(id)key value:(id)value;
- (id)removeForKey:(id)key;
- (id)pollFirst;
+ (ODClassType*)type;
@end


@interface CNTreeMapEntry : NSObject
@property (nonatomic, retain) id key;
@property (nonatomic, retain) id value;
@property (nonatomic, retain) CNTreeMapEntry* left;
@property (nonatomic, retain) CNTreeMapEntry* right;
@property (nonatomic) NSInteger color;
@property (nonatomic, weak) CNTreeMapEntry* parent;

+ (id)treeMapEntry;
- (id)init;
- (ODClassType*)type;
+ (CNTreeMapEntry*)newWithKey:(id)key value:(id)value parent:(CNTreeMapEntry*)parent;
- (CNTreeMapEntry*)next;
+ (ODClassType*)type;
@end


@protocol CNTreeMapKeySet<CNIterable>
- (id<CNIterator>)iteratorHigherThanKey:(id)key;
@end


@interface CNImTreeMapKeySet : NSObject<CNTreeMapKeySet>
@property (nonatomic, readonly, weak) CNTreeMap* map;

+ (id)imTreeMapKeySetWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNIterator>)iteratorHigherThanKey:(id)key;
+ (ODClassType*)type;
@end


@interface CNTreeMapKeyIterator : NSObject<CNIterator>
@property (nonatomic, readonly) CNTreeMap* map;
@property (nonatomic, retain) CNTreeMapEntry* entry;

+ (id)treeMapKeyIteratorWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (CNTreeMapKeyIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNMTreeMapKeySet : NSObject<CNTreeMapKeySet>
@property (nonatomic, readonly, weak) CNMTreeMap* map;

+ (id)treeMapKeySetWithMap:(CNMTreeMap*)map;
- (id)initWithMap:(CNMTreeMap*)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNMutableIterator>)mutableIterator;
- (id<CNIterator>)iteratorHigherThanKey:(id)key;
+ (ODClassType*)type;
@end


@interface CNMTreeMapKeyIterator : NSObject<CNMutableIterator>
@property (nonatomic, readonly) CNMTreeMap* map;
@property (nonatomic, retain) CNTreeMapEntry* entry;

+ (id)treeMapKeyIteratorWithMap:(CNMTreeMap*)map;
- (id)initWithMap:(CNMTreeMap*)map;
- (ODClassType*)type;
+ (CNMTreeMapKeyIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
- (void)remove;
+ (ODClassType*)type;
@end


@interface CNTreeMapValues : NSObject<CNIterable>
@property (nonatomic, readonly, weak) CNTreeMap* map;

+ (id)treeMapValuesWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
+ (ODClassType*)type;
@end


@interface CNTreeMapValuesIterator : NSObject<CNIterator>
@property (nonatomic, readonly) CNTreeMap* map;
@property (nonatomic, retain) CNTreeMapEntry* entry;

+ (id)treeMapValuesIteratorWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (CNTreeMapValuesIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNTreeMapIterator : NSObject<CNIterator>
@property (nonatomic, readonly) CNTreeMap* map;
@property (nonatomic, retain) CNTreeMapEntry* entry;

+ (id)treeMapIteratorWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (CNTreeMapIterator*)applyMap:(CNTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNMTreeMapIterator : NSObject<CNMutableIterator>
@property (nonatomic, readonly) CNMTreeMap* map;
@property (nonatomic, retain) CNTreeMapEntry* entry;

+ (id)treeMapIteratorWithMap:(CNMTreeMap*)map;
- (id)initWithMap:(CNMTreeMap*)map;
- (ODClassType*)type;
+ (CNMTreeMapIterator*)applyMap:(CNMTreeMap*)map entry:(CNTreeMapEntry*)entry;
- (BOOL)hasNext;
- (id)next;
- (void)remove;
+ (ODClassType*)type;
@end


