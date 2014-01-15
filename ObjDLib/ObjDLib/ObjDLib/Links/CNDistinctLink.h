#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNDistinctLink : NSObject <CNChainLink>
- (id)initWithSelectivity:(double)selectivity;
+ (id)linkWithSelectivity:(double)selectivity;
@end