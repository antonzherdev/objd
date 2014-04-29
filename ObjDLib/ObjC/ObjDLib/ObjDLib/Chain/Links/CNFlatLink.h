#import "objdcore.h"
#import "CNObject.h"
#import "CNChain.h"
@class CNYield;
@protocol CNTraversable;
@class CNClassType;

@class CNFlatLink;

@interface CNFlatLink : NSObject<CNChainLink> {
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


