#import "objd.h"
#import "CNYield.h"

@class CNSortLink;
@class CNReverseLink;
@class CNShuffleLink;

@interface CNSortLink : CNChainLink_impl {
@public
    NSInteger(^_comparator)(id, id);
}
@property (nonatomic, readonly) NSInteger(^comparator)(id, id);

+ (instancetype)sortLinkWithComparator:(NSInteger(^)(id, id))comparator;
- (instancetype)initWithComparator:(NSInteger(^)(id, id))comparator;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNReverseLink : CNChainLink_impl
+ (instancetype)reverseLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNShuffleLink : CNChainLink_impl {
@public
    CNMArray* __array;
}
+ (instancetype)shuffleLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


