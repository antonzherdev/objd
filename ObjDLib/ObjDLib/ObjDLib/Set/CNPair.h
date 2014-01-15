#import "objdcore.h"
#import "CNSet.h"
#import "CNCollection.h"
@class ODClassType;
@class CNChain;

@class CNPair;
@class CNPairIterator;

@interface CNPair : NSObject<CNSet>
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;

+ (id)pairWithA:(id)a b:(id)b;
- (id)initWithA:(id)a b:(id)b;
- (ODClassType*)type;
+ (CNPair*)newWithA:(id)a b:(id)b;
- (BOOL)containsItem:(id)item;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)head;
- (id)headOpt;
+ (ODClassType*)type;
@end


@interface CNPairIterator : NSObject<CNIterator>
@property (nonatomic, readonly) CNPair* pair;

+ (id)pairIteratorWithPair:(CNPair*)pair;
- (id)initWithPair:(CNPair*)pair;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


