#import <Foundation/Foundation.h>

#import "CNSet.h"

@interface NSMutableSet (CNChain)<CNMSet>
+ (id) hashSet;
- (NSSet*)im;

+ (NSMutableSet *)applyCapacity:(NSUInteger)capacity;

- (NSSet*)imCopy;
@end