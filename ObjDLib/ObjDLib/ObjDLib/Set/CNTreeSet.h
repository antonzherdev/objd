#import "objdcore.h"
#import "CNSet.h"
#import "CNCollection.h"
@class CNTreeMap;
@protocol CNTreeMapKeySet;
@class ODClassType;
@class CNDispatchQueue;
@class CNChain;
@class CNImTreeMap;
@class NSObject;
@class CNMTreeMap;
@class CNMTreeMapKeySet;
@class ODObject;

@class CNTreeSet;
@class CNImTreeSet;
@class CNTreeSetBuilder;
@class CNMTreeSet;

@interface CNTreeSet : NSObject<CNSet> {
@protected
    CNTreeMap* _map;
}
@property (nonatomic, readonly) CNTreeMap* map;

+ (instancetype)treeSetWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (ODClassType*)type;
- (id)higherThanItem:(id)item;
- (id)lowerThanItem:(id)item;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNIterator>)iteratorHigherThanItem:(id)item;
- (id)head;
- (id)last;
- (BOOL)containsItem:(id)item;
+ (ODClassType*)type;
@end


@interface CNImTreeSet : CNTreeSet<CNImSet> {
@protected
    CNImTreeMap* _immap;
}
@property (nonatomic, readonly) CNImTreeMap* immap;

+ (instancetype)imTreeSetWithImmap:(CNImTreeMap*)immap;
- (instancetype)initWithImmap:(CNImTreeMap*)immap;
- (ODClassType*)type;
- (CNMTreeSet*)mCopy;
+ (ODClassType*)type;
@end


@interface CNTreeSetBuilder : NSObject<CNBuilder> {
@protected
    NSInteger(^_comparator)(id, id);
    CNMTreeSet* _set;
}
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (instancetype)treeSetBuilderWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (ODClassType*)type;
+ (CNTreeSetBuilder*)apply;
- (void)appendItem:(id)item;
- (CNImTreeSet*)build;
+ (ODClassType*)type;
@end


@interface CNMTreeSet : CNTreeSet<CNMSet> {
@protected
    CNMTreeMap* _mmap;
}
@property (nonatomic, readonly) CNMTreeMap* mmap;

+ (instancetype)treeSetWithMmap:(CNMTreeMap*)mmap;
- (instancetype)initWithMmap:(CNMTreeMap*)mmap;
- (ODClassType*)type;
+ (CNMTreeSet*)applyComparator:(NSInteger(^)(id, id))comparator;
+ (CNMTreeSet*)apply;
- (id<CNMIterator>)mutableIterator;
- (void)appendItem:(id)item;
- (BOOL)removeItem:(id)item;
- (void)clear;
- (void)addAllObjects:(id<CNTraversable>)objects;
- (CNMTreeSet*)reorder;
- (CNImTreeSet*)im;
- (CNImTreeSet*)imCopy;
+ (ODClassType*)type;
@end


