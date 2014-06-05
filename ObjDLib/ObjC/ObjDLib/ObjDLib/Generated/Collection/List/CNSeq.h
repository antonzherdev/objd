#import "objdcore.h"
#import "CNCollection.h"
#import "CNObject.h"
@protocol CNSet;
@class CNHashSetBuilder;
@class CNClassType;
@class CNString;
@class CNDispatchQueue;
@class CNChain;
@class CNMArray;
@class CNImArray;

@class CNSeq_impl;
@class CNImSeq_impl;
@class CNMSeq_impl;
@class CNArrayBuilder;
@class CNIndexFunSeq;
@class CNIndexFunSeqIterator;
@protocol CNSeq;
@protocol CNImSeq;
@protocol CNMSeq;

@protocol CNSeq<CNIterable>
- (id)applyIndex:(NSUInteger)index;
- (id<CNSet>)toSet;
- (BOOL)isEqualSeq:(id<CNSeq>)seq;
- (BOOL)isEmpty;
- (id)head;
- (id)last;
- (id<CNImSeq>)tail;
- (NSString*)description;
- (BOOL)isEqual:(id)to;
@end


@interface CNSeq_impl : CNIterable_impl<CNSeq>
+ (instancetype)seq_impl;
- (instancetype)init;
- (BOOL)isEmpty;
- (id)head;
@end


@protocol CNImSeq<CNSeq, CNImIterable>
- (id<CNImSeq>)addItem:(id)item;
- (id<CNImSeq>)addSeq:(id<CNSeq>)seq;
- (id<CNImSeq>)subItem:(id)item;
- (id<CNMSeq>)mCopy;
- (NSString*)description;
@end


@interface CNImSeq_impl : CNSeq_impl<CNImSeq>
+ (instancetype)imSeq_impl;
- (instancetype)init;
- (id<CNMSeq>)mCopy;
@end


@protocol CNMSeq<CNSeq, CNMIterable>
- (BOOL)removeIndex:(NSUInteger)index;
- (void)insertIndex:(NSUInteger)index item:(id)item;
- (void)prependItem:(id)item;
- (void)setIndex:(NSUInteger)index item:(id)item;
- (id<CNImSeq>)im;
- (id<CNImSeq>)imCopy;
- (NSString*)description;
@end


@interface CNMSeq_impl : CNSeq_impl<CNMSeq>
+ (instancetype)seq_impl;
- (instancetype)init;
- (id<CNImSeq>)im;
- (id<CNImSeq>)imCopy;
@end


@interface CNArrayBuilder : CNBuilder_impl {
@public
    CNMArray* _array;
}
+ (instancetype)arrayBuilderWithCapacity:(NSUInteger)capacity;
- (instancetype)initWithCapacity:(NSUInteger)capacity;
- (CNClassType*)type;
- (void)appendItem:(id)item;
- (CNImArray*)build;
+ (CNArrayBuilder*)apply;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNIndexFunSeq : CNImSeq_impl {
@public
    NSUInteger _count;
    id(^_f)(NSUInteger);
}
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) id(^f)(NSUInteger);

+ (instancetype)indexFunSeqWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (CNClassType*)type;
- (id)applyIndex:(NSUInteger)index;
- (id<CNIterator>)iterator;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNIndexFunSeqIterator : CNIterator_impl {
@public
    NSUInteger _count;
    id(^_f)(NSUInteger);
    NSUInteger _i;
}
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) id(^f)(NSUInteger);

+ (instancetype)indexFunSeqIteratorWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (NSString*)description;
+ (CNClassType*)type;
@end


