#import "objdcore.h"
#import "CNSet.h"
#import "CNCollection.h"
@class CNTreeMap;
@protocol CNTreeMapKeySet;
@class ODClassType;
@class CNChain;
@class NSObject;
@class CNMTreeMap;
@class CNMTreeMapKeySet;

@class CNTreeSet;
@class CNImTreeSet;
@class CNTreeSetBuilder;
@class CNMTreeSet;

@interface CNTreeSet : NSObject<CNSet>
@property (nonatomic, readonly) CNTreeMap* map;

+ (id)treeSetWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
- (id)higherThanItem:(id)item;
- (id)lowerThanItem:(id)item;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNIterator>)iteratorHigherThanItem:(id)item;
- (id)head;
- (id)headOpt;
- (id)last;
- (BOOL)containsItem:(id)item;
+ (ODClassType*)type;
@end


@interface CNImTreeSet : CNTreeSet
+ (id)imTreeSetWithMap:(CNTreeMap*)map;
- (id)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
+ (ODClassType*)type;
@end


@interface CNTreeSetBuilder : NSObject<CNBuilder>
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (id)treeSetBuilderWithComparator:(NSInteger(^)(id, id))comparator;
- (id)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
+ (CNTreeSetBuilder*)apply;
- (void)appendItem:(id)item;
- (CNTreeSet*)build;
+ (ODClassType*)type;
@end


@interface CNMTreeSet : CNTreeSet<CNMutableSet>
@property (nonatomic, readonly) CNMTreeMap* mmap;

+ (id)treeSetWithMmap:(CNMTreeMap*)mmap;
- (id)initWithMmap:(CNMTreeMap*)mmap;
- (ODClassType*)type;
+ (CNMTreeSet*)applyComparator:(NSInteger(^)(id, id))comparator;
+ (CNMTreeSet*)apply;
- (id<CNMutableIterator>)mutableIterator;
- (void)appendItem:(id)item;
- (BOOL)removeItem:(id)item;
- (void)clear;
- (void)addAllObjects:(id<CNTraversable>)objects;
- (CNMTreeSet*)reorder;
+ (ODClassType*)type;
@end


