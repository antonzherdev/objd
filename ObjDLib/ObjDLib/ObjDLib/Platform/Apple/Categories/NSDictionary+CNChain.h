#import <Foundation/Foundation.h>
#import "CNTypes.h"
#import "CNMap.h"

@class CNChain;

@interface NSDictionary (CNChain) <CNImMap>
- (NSDictionary *)dictionaryByAddingValue:(id)value forKey:(id)key;
- (CNChain *) chain;
- (void) forEach:(cnP)p;
- (BOOL) goOn:(BOOL(^)(id))on;
@end