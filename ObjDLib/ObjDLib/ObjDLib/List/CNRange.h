#import "objdcore.h"
#import "CNSeq.h"
#import "CNCollection.h"
@class ODClassType;
@class CNChain;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNDispatchQueue;

@class CNRange;
@class CNRangeIterator;

@interface CNRange : NSObject<CNImSeq> {
@protected
    NSInteger _start;
    NSInteger _end;
    NSInteger _step;
    NSUInteger _count;
}
@property (nonatomic, readonly) NSInteger start;
@property (nonatomic, readonly) NSInteger end;
@property (nonatomic, readonly) NSInteger step;
@property (nonatomic, readonly) NSUInteger count;

+ (instancetype)rangeWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (instancetype)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (ODClassType*)type;
- (id)applyIndex:(NSUInteger)index;
- (id<CNIterator>)iterator;
- (CNRange*)setStep:(NSInteger)step;
- (BOOL)isEmpty;
+ (CNRange*)applyI:(NSInteger)i;
+ (ODClassType*)type;
@end


@interface CNRangeIterator : NSObject<CNIterator> {
@protected
    NSInteger _start;
    NSInteger _end;
    NSInteger _step;
    NSInteger _i;
}
@property (nonatomic, readonly) NSInteger start;
@property (nonatomic, readonly) NSInteger end;
@property (nonatomic, readonly) NSInteger step;

+ (instancetype)rangeIteratorWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (instancetype)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


