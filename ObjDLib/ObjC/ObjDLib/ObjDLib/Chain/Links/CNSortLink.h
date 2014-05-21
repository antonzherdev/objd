#import "objdcore.h"
#import "CNYield.h"
#import "CNCollection.h"
#import "CNObject.h"
@class CNMTreeSet;
@class CNClassType;
@class CNMList;
@class CNMArray;

@class CNSortLink;
@class CNReverseLink;
@class CNShuffleLink;

@interface CNSortLink : CNChainLink_impl {
@protected
    NSInteger(^_comparator)(id, id);
}
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (instancetype)sortLinkWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNReverseLink : CNChainLink_impl
+ (instancetype)reverseLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNShuffleLink : CNChainLink_impl {
@protected
    CNMArray* __array;
}
+ (instancetype)shuffleLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


