#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNMapLink : NSObject <CNChainLink>
- (id)initWithF:(cnF)f;

+ (id)linkWithF:(cnF)f;

@end