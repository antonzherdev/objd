#import <Foundation/Foundation.h>

#import "CNSet.h"

@interface NSMutableSet (CNChain)<CNMSet>
+ (id) mutableSet;
- (NSSet*)im;
- (NSSet*)imCopy;
@end