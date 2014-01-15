#import <Foundation/Foundation.h>
#import "CNSeq.h"

@interface NSMutableArray (CNChain) <CNMutableSeq>
+ (NSMutableArray *)mutableArray;
@end