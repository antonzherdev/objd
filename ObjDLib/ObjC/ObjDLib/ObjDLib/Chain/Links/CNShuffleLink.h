#import "objdcore.h"
#import "CNObject.h"
#import "CNChain.h"
@class CNMArray;
@class CNYield;
@class CNClassType;

@class CNShuffleLink;

@interface CNShuffleLink : NSObject<CNChainLink> {
@protected
    CNMArray* __array;
}
+ (instancetype)shuffleLink;
- (instancetype)init;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


