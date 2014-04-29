#import "objdcore.h"
#import "CNSet.h"
#import "CNCollection.h"
@class CNClassType;

@class CNPair;
@class CNPairIterator;

@interface CNPair : CNImSet_impl {
@protected
    id _a;
    id _b;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;

+ (instancetype)pairWithA:(id)a b:(id)b;
- (instancetype)initWithA:(id)a b:(id)b;
- (CNClassType*)type;
+ (CNPair*)newWithA:(id)a b:(id)b;
- (BOOL)containsItem:(id)item;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)head;
+ (CNClassType*)type;
@end


@interface CNPairIterator : CNIterator_impl {
@protected
    CNPair* _pair;
    NSInteger _state;
}
@property (nonatomic, readonly) CNPair* pair;

+ (instancetype)pairIteratorWithPair:(CNPair*)pair;
- (instancetype)initWithPair:(CNPair*)pair;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (CNClassType*)type;
@end


