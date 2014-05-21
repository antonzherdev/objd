#import <Foundation/Foundation.h>
#import "CNTypes.h"
#import "CNList.h"

@class CNChain;

@interface NSArray (CNChain)<CNImSeq>
- (id) chain:(cnChainBuildBlock)block;
- (CNChain*) chain;

- (id) head;
- (id) randomItem;
- (void) forEach:(cnP)p;
- (id)findWhere:(cnPredicate)predicate;
- (NSArray *)arrayByRemovingObject:(id)object;
- (id <CNSet>)toSet;
- (NSArray*)addItem:(id)item;
- (NSArray*)addSeq:(id<CNSeq>)seq;
- (NSArray*)subItem:(id)item;
- (NSArray*)tail;
@end
