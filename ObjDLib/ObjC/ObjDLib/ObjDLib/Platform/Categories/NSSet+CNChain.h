#import <Foundation/Foundation.h>
#import "CNTypes.h"
#import "CNCollection.h"
#import "CNSet.h"

@class CNChain;


@interface NSSet (CNChain) <CNImSet>
+ (id <CNSet>)imHashSet;
@end