#import <Foundation/Foundation.h>

#import "CNSet.h"

@interface NSMutableSet (CNChain)<CNMSet>
+ (id) hashSet;
- (NSSet*)im;
- (NSSet*)imCopy;
@end