#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNNeighboursLink : NSObject <CNChainLink>
- (id)initWithRing:(BOOL)ring;

+ (id)linkWithRing:(BOOL)ring;
@end