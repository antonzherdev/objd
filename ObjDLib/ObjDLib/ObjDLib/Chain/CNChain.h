#import <Foundation/Foundation.h>
#import "CNTypes.h"
#import "CNCollection.h"

@class CNSortBuilder;
@protocol CNTraversable;
@class CNTreeSet;

@interface CNChain : NSObject <CNTraversable>
- (id)initWithLink:(id <CNChainLink>)link previous:(CNChain *)previous;

+ (id)chainWithLink:(id <CNChainLink>)link previous:(CNChain *)previous;

+ (CNChain*)chainWithCollection:(id)collection;

- (CNChain*)link:(id<CNChainLink>)link;
- (CNChain*)filter:(cnPredicate)predicate;
- (CNChain*)filter:(cnPredicate)predicate selectivity:(double)selectivity;
- (CNChain*)filterCast:(ODType *)type;


- (CNChain*)map:(cnF)f;
- (CNChain*)flatMap:(cnF)f;
- (CNChain*)flatMap:(cnF)f factor:(double) factor;
- (CNChain*)neighbors;
- (CNChain*)neighborsRing;
- (CNChain*)combinations;
- (CNChain*)uncombinations;

- (CNChain*)groupBy:(cnF)by fold:(cnF2)fold withStart:(cnF0)start;
- (CNChain*)groupBy:(cnF)by withBuilder:(cnF0)builder;
- (CNChain*)groupBy:(cnF)by map:(cnF)f withBuilder:(cnF0)builder;
- (CNChain*)groupBy:(cnF)by map:(cnF)f;
- (CNChain*)groupBy:(cnF)by;

- (CNChain*)zipA:(id <CNIterable>)a;
- (CNChain*)zipA:(id <CNIterable>)a by:(cnF2)by;
- (CNChain*)zip3A:(id <CNIterable>)a b:(id <CNIterable>)b;
- (CNChain*)zip3A:(id <CNIterable>)a b:(id <CNIterable>)b by:(cnF3)by;

- (CNChain*)append:(id)collection;
- (CNChain*)prepend:(id)collection;
- (CNChain*)exclude:(id)collection;
- (CNChain*)intersect:(id)collection;

- (CNChain*)mul :(id)collection;

- (CNChain*)reverse;
- (CNChain *)distinct;
- (CNChain *)distinctWithSelectivity:(double) selectivity;
- (CNChain *)sort;
- (CNChain *)sort:(cnCompare)comparator;
- (CNChain *)sortDesc;
- (CNSortBuilder *)sortBy;


- (id)randomItem;
- (NSUInteger)count;
- (NSArray*)toArray;
- (NSSet*)toSet;

- (id)foldStart:(id)start by:(cnF2)by;
- (id)min;
- (id)max;
- (NSDictionary *)toMap;
- (NSMutableDictionary *)toMutableMap;
- (CNYieldResult)apply:(CNYield *)yield;
- (NSString *)toStringWithDelimiter:(NSString *)delimiter;

- (NSString *)toStringWithStart:(NSString *)string delimiter:(NSString *)delimiter end:(NSString *)end;

- (NSString *)charsToString;

- (CNTreeSet *)toTreeSet;

- (CNChain *)topNumbers:(NSUInteger)numbers;
@end
