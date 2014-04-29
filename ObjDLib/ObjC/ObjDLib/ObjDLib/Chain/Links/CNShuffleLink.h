#import "objdcore.h"
#import "CNYield.h"
#import "CNObject.h"
@class CNMArray;
@class CNClassType;

@class CNShuffleLink;

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


