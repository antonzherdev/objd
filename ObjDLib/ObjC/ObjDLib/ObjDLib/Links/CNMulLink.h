#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNMulLink : NSObject <CNChainLink>
- (id)initWithCollection:(id)collection;

+ (id)linkWithCollection:(id)collection;

@end