#import <Foundation/Foundation.h>
#import "CNMap.h"
#import "NSDictionary+CNChain.h"

@interface NSMutableDictionary (CNChain) <CNMMap, CNHashMap>
+ (NSMutableDictionary *)hashMap;
- (NSDictionary*)im;
- (NSDictionary*)imCopy;
@end