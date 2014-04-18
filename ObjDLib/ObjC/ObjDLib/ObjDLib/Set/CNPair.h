#import "objdcore.h"
#import "CNSet.h"
#import "CNCollection.h"
@class ODClassType;
@class CNMHashSet;
@class CNDispatchQueue;
@class CNChain;
@class CNMArray;

@class CNPair;
@class CNPairIterator;

@interface CNPair : NSObject<CNImSet> {
@protected
    id _a;
    id _b;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;

+ (instancetype)pairWithA:(id)a b:(id)b;
- (instancetype)initWithA:(id)a b:(id)b;
- (ODClassType*)type;
+ (CNPair*)newWithA:(id)a b:(id)b;
- (BOOL)containsItem:(id)item;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)head;
+ (ODClassType*)type;
@end


@interface CNPairIterator : NSObject<CNIterator> {
@protected
    CNPair* _pair;
    NSInteger _state;
}
@property (nonatomic, readonly) CNPair* pair;

+ (instancetype)pairIteratorWithPair:(CNPair*)pair;
- (instancetype)initWithPair:(CNPair*)pair;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


