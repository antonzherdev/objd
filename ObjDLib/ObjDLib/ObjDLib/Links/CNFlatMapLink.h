#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNFlatMapLink : NSObject <CNChainLink>
- (id)initWithF:(cnF)f factor:(double)factor;

+ (id)linkWithF:(cnF)f factor:(double)factor;

@end