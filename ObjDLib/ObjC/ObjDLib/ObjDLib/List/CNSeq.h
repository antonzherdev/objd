#import "objdcore.h"
#import "CNCollection.h"
@protocol CNSet;
@class CNHashSetBuilder;
@class ODClassType;
@class CNDispatchQueue;
@class CNChain;

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
@end


@protocol CNImSeq<CNSeq, CNImIterable>
- (id<CNImSeq>)addItem:(id)item;
- (id<CNImSeq>)addSeq:(id<CNSeq>)seq;
- (id<CNImSeq>)subItem:(id)item;
- (id<CNMSeq>)mCopy;
@end


@protocol CNMSeq<CNSeq, CNMIterable>
- (BOOL)removeIndex:(NSUInteger)index;
- (void)insertIndex:(NSUInteger)index item:(id)item;
- (void)prependItem:(id)item;
- (void)setIndex:(NSUInteger)index item:(id)item;
- (id<CNImSeq>)im;
- (id<CNImSeq>)imCopy;
@end


@interface CNArrayBuilder : NSObject<CNBuilder> {
@protected
    NSMutableArray* _array;
}
+ (instancetype)arrayBuilder;
- (instancetype)init;
- (ODClassType*)type;
- (void)appendItem:(id)item;
- (NSArray*)build;
+ (ODClassType*)type;
@end


@interface CNIndexFunSeq : NSObject<CNImSeq> {
@protected
    NSUInteger _count;
    id(^_f)(NSUInteger);
}
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) id(^f)(NSUInteger);

+ (instancetype)indexFunSeqWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (ODClassType*)type;
- (id)applyIndex:(NSUInteger)index;
- (id<CNIterator>)iterator;
+ (ODClassType*)type;
@end


@interface CNIndexFunSeqIterator : NSObject<CNIterator> {
@protected
    NSUInteger _count;
    id(^_f)(NSUInteger);
    NSUInteger _i;
}
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) id(^f)(NSUInteger);

+ (instancetype)indexFunSeqIteratorWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


