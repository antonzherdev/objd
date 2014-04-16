#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNAppendLink : NSObject <CNChainLink>
- (id)initWithCollection:(id)collection;

+ (id)linkWithCollection:(id)collection;

@end