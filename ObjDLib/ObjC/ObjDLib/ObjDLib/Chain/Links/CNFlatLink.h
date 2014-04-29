#import "objdcore.h"
#import "CNYield.h"
@protocol CNTraversable;
@class CNClassType;

@class CNFlatLink;

@interface CNFlatLink : CNChainLink_impl {
@protected
    CGFloat _factor;
}
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)flatLinkWithFactor:(CGFloat)factor;
- (instancetype)initWithFactor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


