#import "objdcore.h"
#import "CNYield.h"
@protocol CNTraversable;
@class CNChain;
@protocol CNIterable;
@class CNClassType;

@class CNMulLink;

@interface CNMulLink : CNChainLink_impl {
@protected
    id<CNTraversable> __collection;
}
+ (instancetype)mulLinkWithCollection:(id<CNTraversable>)collection;
- (instancetype)initWithCollection:(id<CNTraversable>)collection;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


