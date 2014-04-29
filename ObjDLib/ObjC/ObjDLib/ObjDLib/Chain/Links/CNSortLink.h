#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNSortLink : NSObject <CNChainLink>
- (id)initWithComparator:(cnCompare)comparator;

+ (id)linkWithComparator:(cnCompare)comparator;

+ (id)link;
@end