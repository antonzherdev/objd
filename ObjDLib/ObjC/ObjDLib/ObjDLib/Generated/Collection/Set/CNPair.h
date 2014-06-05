#import "objdcore.h"
#import "CNSet.h"
#import "CNObject.h"
#import "CNCollection.h"
@class CNClassType;
@class CNString;

@class CNPair;
@class CNPairIterator;

@interface CNPair : CNImSet_impl {
@public
    id _a;
    id _b;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;

+ (instancetype)pairWithA:(id)a b:(id)b;
- (instancetype)initWithA:(id)a b:(id)b;
- (CNClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)head;
- (BOOL)isEqualPair:(CNPair*)pair;
- (NSUInteger)hash;
- (NSString*)description;
- (BOOL)isEqual:(id)to;
+ (CNClassType*)type;
@end


@interface CNPairIterator : CNIterator_impl {
@public
    CNPair* _pair;
    NSInteger _state;
}
@property (nonatomic, readonly) CNPair* pair;

+ (instancetype)pairIteratorWithPair:(CNPair*)pair;
- (instancetype)initWithPair:(CNPair*)pair;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (NSString*)description;
+ (CNClassType*)type;
@end


