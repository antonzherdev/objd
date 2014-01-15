#import <Foundation/Foundation.h>


@interface CNFilterLink : NSObject <CNChainLink>
- (id)initWithPredicate:(cnPredicate)predicate selectivity:(double)selectivity;

+ (id)linkWithPredicate:(cnPredicate)predicate selectivity:(double)selectivity;
@end