#import <Foundation/Foundation.h>
#import "CNMap.h"

@interface NSMutableDictionary (CNChain) <CNMMap>
+ (NSMutableDictionary *)hashMap;
- (NSDictionary*)im;
- (NSDictionary*)imCopy;
@end