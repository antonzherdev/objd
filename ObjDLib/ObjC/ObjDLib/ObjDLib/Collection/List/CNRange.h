#import "objdcore.h"
#import "CNSeq.h"
#import "CNCollection.h"
@class CNClassType;

@class CNRange;
@class CNRangeIterator;

@interface CNRange : CNImSeq_impl {
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
- (CNClassType*)type;
- (id)applyIndex:(NSUInteger)index;
- (id<CNIterator>)iterator;
- (CNRange*)setStep:(NSInteger)step;
- (BOOL)isEmpty;
+ (CNRange*)applyI:(NSInteger)i;
+ (CNClassType*)type;
@end


@interface CNRangeIterator : CNIterator_impl {
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
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (CNClassType*)type;
@end


