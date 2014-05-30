#import "objdcore.h"
#import "CNSet.h"
#import "CNObject.h"
#import "CNCollection.h"
@class CNTreeMap;
@protocol CNTreeMapKeySet;
@class CNClassType;
@class CNString;
@class CNImTreeMap;
@class CNMTreeMap;
@class CNMTreeMapKeySet;

@class CNTreeSet;
@class CNImTreeSet;
@class CNTreeSetBuilder;
@class CNMTreeSet;

@interface CNTreeSet : CNSet_impl {
@protected
    CNTreeMap* _map;
}
@property (nonatomic, readonly) CNTreeMap* map;

+ (instancetype)treeSetWithMap:(CNTreeMap*)map;
- (instancetype)initWithMap:(CNTreeMap*)map;
- (CNClassType*)type;
- (id)higherThanItem:(id)item;
- (id)lowerThanItem:(id)item;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNIterator>)iteratorHigherThanItem:(id)item;
- (id)head;
- (id)last;
- (BOOL)containsItem:(id)item;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNImTreeSet : CNTreeSet<CNImSet> {
@protected
    CNImTreeMap* _immap;
}
@property (nonatomic, readonly) CNImTreeMap* immap;

+ (instancetype)imTreeSetWithImmap:(CNImTreeMap*)immap;
- (instancetype)initWithImmap:(CNImTreeMap*)immap;
- (CNClassType*)type;
- (CNMTreeSet*)mCopy;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNTreeSetBuilder : CNBuilder_impl {
@protected
    NSInteger(^_comparator)(id, id);
    CNMTreeSet* _set;
}
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (instancetype)treeSetBuilderWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (CNClassType*)type;
+ (CNTreeSetBuilder*)apply;
- (void)appendItem:(id)item;
- (CNImTreeSet*)build;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNMTreeSet : CNTreeSet<CNMSet> {
@protected
    CNMTreeMap* _mmap;
}
@property (nonatomic, readonly) CNMTreeMap* mmap;

+ (instancetype)treeSetWithMmap:(CNMTreeMap*)mmap;
- (instancetype)initWithMmap:(CNMTreeMap*)mmap;
- (CNClassType*)type;
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
- (NSString*)description;
+ (CNClassType*)type;
@end


