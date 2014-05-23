#import "objdcore.h"
#import "CNObject.h"
#import "CNCollection.h"
@class CNClassType;
@protocol CNChainLink;
@class CNSourceLink;
@class CNFilterLink;
@class CNTopLink;
@class CNMapLink;
@class CNMapOptLink;
@class CNFlatMapLink;
@class CNFlatLink;
@class CNCombinationsLink;
@class CNUncombinationsLink;
@class CNNeighboursLink;
@class CNMulLink;
@class CNArrayBuilder;
@class CNMGroupByLink;
@class CNImGroupByLink;
@class CNDistinctLink;
@class CNZipLink;
@class CNYield;
@class CNZip3Link;
@class CNPrependLink;
@class CNAppendLink;
@class CNReverseLink;
@class CNSortLink;
@class CNSortBuilder;
@class CNShuffleLink;
@protocol CNSeq;
@class CNSeed;
@class CNType;
@class CNMArray;
@class CNImList;
@class CNImListBuilder;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNTreeSet;
@class CNTreeSetBuilder;
@class CNHashMapBuilder;
@class CNMHashMap;
@class CNImHashMap;
@class CNStringBuilder;
@class CNFuture;
@class CNFutureEnd;
@class CNFutureVoidEnd;

@class CNChain;



@interface CNChain : CNImTraversable_impl {
@protected
    id<CNChainLink> _link;
    CNChain* _previous;
}
@property (nonatomic, readonly) id<CNChainLink> link;
@property (nonatomic, readonly) CNChain* previous;

+ (instancetype)chainWithLink:(id<CNChainLink>)link previous:(CNChain*)previous;
- (instancetype)initWithLink:(id<CNChainLink>)link previous:(CNChain*)previous;
- (CNClassType*)type;
+ (CNChain*)applyCollection:(id<CNTraversable>)collection;
- (CNChain*)filterFactor:(CGFloat)factor when:(BOOL(^)(id))when;
- (CNChain*)filterWhen:(BOOL(^)(id))when;
- (CNChain*)topNumbers:(NSInteger)numbers;
- (CNChain*)filterCastFactor:(CGFloat)factor to:(CNClassType*)to;
- (CNChain*)filterCastTo:(CNClassType*)to;
- (CNChain*)mapF:(id(^)(id))f;
- (CNChain*)mapOptF:(id(^)(id))f;
- (CNChain*)flatMapFactor:(CGFloat)factor f:(id<CNTraversable>(^)(id))f;
- (CNChain*)flatMapF:(id<CNTraversable>(^)(id))f;
- (CNChain*)flatFactor:(CGFloat)factor;
- (CNChain*)flat;
- (CNChain*)combinations;
- (CNChain*)uncombinations;
- (CNChain*)neighbours;
- (CNChain*)neighboursRing;
- (CNChain*)mulBy:(id<CNTraversable>)by;
- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by;
- (CNChain*)groupBy:(id(^)(id))by;
- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by f:(id(^)(id))f;
- (CNChain*)groupBy:(id(^)(id))by f:(id(^)(id))f;
- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by builder:(id<CNBuilder>(^)())builder;
- (CNChain*)groupBy:(id(^)(id))by builder:(id<CNBuilder>(^)())builder;
- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by f:(id(^)(id))f builder:(id<CNBuilder>(^)())builder;
- (CNChain*)groupBy:(id(^)(id))by f:(id(^)(id))f builder:(id<CNBuilder>(^)())builder;
- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold;
- (CNChain*)groupBy:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold;
- (CNChain*)distinctFactor:(CGFloat)factor;
- (CNChain*)distinct;
- (CNChain*)zipB:(id<CNIterable>)b;
- (CNChain*)zipB:(id<CNIterable>)b by:(id(^)(id, id))by;
- (void)zipForB:(id<CNIterable>)b by:(void(^)(id, id))by;
- (CNChain*)zip3B:(id<CNIterable>)b c:(id<CNIterable>)c;
- (CNChain*)zip3B:(id<CNIterable>)b c:(id<CNIterable>)c by:(id(^)(id, id, id))by;
- (CNChain*)prependCollection:(id<CNTraversable>)collection;
- (CNChain*)appendCollection:(id<CNTraversable>)collection;
- (CNChain*)excludeCollection:(id<CNTraversable>)collection;
- (CNChain*)intersectCollection:(id<CNIterable>)collection;
- (CNChain*)reverse;
- (CNChain*)reverseWhen:(BOOL)when;
- (CNChain*)sort;
- (CNChain*)sortDesc;
- (CNChain*)sortComparator:(NSInteger(^)(id, id))comparator;
- (CNSortBuilder*)sortBy;
- (CNChain*)shuffle;
- (CNGoR)goOn:(CNGoR(^)(id))on;
- (id)foldStart:(id)start by:(id(^)(id, id))by;
- (NSUInteger)count;
- (id)last;
- (id)randomItem;
- (id)randomItemSeed:(CNSeed*)seed;
- (BOOL)isEmpty;
- (CNTuple*)gap;
- (id)min;
- (id)max;
- (BOOL)or;
- (BOOL)and;
- (id<CNSeq>)toSeq;
- (NSArray*)toArray;
- (CNImList*)toList;
- (id<CNSet>)toSet;
- (CNTreeSet*)toTreeSet;
- (NSDictionary*)toMap;
- (NSString*)toStringStart:(NSString*)start delimiter:(NSString*)delimiter end:(NSString*)end;
- (NSString*)toStringDelimiter:(NSString*)delimiter;
- (CNFuture*)futureF:(id(^)(CNChain*))f;
- (CNFuture*)future;
- (CNFuture*)voidFuture;
- (CNGoR)applyYield:(CNYield*)yield;
- (CNChain*)addLink:(id<CNChainLink>)link;
+ (CNClassType*)type;
@end


