#import <Foundation/Foundation.h>
#import "CNMap.h"

@interface NSMutableDictionary (CNChain) <CNMMap>
- (id)objectForKey:(id)key orUpdateWith:(id (^)())with;

+ (NSMutableDictionary *)mutableDictionary;
- (NSDictionary*)im;
- (NSDictionary*)imCopy;
@end