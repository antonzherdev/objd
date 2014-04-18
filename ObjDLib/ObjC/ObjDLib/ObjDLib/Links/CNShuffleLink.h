#import "objdcore.h"
#import "CNChain.h"
#import "ODObject.h"
@class CNMArray;
@class CNYield;
@class ODClassType;

@class CNShuffleLink;

@interface CNShuffleLink : NSObject<CNChainLink> {
@protected
    CNMArray* __array;
}
+ (instancetype)shuffleLink;
- (instancetype)init;
- (ODClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (ODClassType*)type;
@end


