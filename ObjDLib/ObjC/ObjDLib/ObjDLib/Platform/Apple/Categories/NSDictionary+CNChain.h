#import <Foundation/Foundation.h>
#import "CNTypes.h"
#import "CNMap.h"

@class CNChain;

@protocol CNHashMap <NSObject, CNMap>
@end

@interface NSDictionary (CNChain) <CNImMap, CNHashMap>
- (NSDictionary *)dictionaryByAddingValue:(id)value forKey:(id)key;
- (CNChain *) chain;
- (void) forEach:(cnP)p;
@end