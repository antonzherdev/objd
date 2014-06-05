#import "objd.h"
#import "CNYield.h"

@class CNCombinationsLink;
@class CNUncombinationsLink;
@class CNNeighboursLink;

@interface CNCombinationsLink : CNChainLink_impl
+ (instancetype)combinationsLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNUncombinationsLink : CNChainLink_impl
+ (instancetype)uncombinationsLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNNeighboursLink : CNChainLink_impl {
@public
    BOOL _ring;
}
@property (nonatomic, readonly) BOOL ring;

+ (instancetype)neighboursLinkWithRing:(BOOL)ring;
- (instancetype)initWithRing:(BOOL)ring;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


