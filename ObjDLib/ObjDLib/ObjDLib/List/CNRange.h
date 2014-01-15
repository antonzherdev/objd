#import "objdcore.h"
#import "CNSeq.h"
#import "ODObject.h"
#import "CNCollection.h"
@class ODClassType;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNChain;

@class CNRange;
@class CNRangeIterator;

@interface CNRange : NSObject<CNSeq>
@property (nonatomic, readonly) NSInteger start;
@property (nonatomic, readonly) NSInteger end;
@property (nonatomic, readonly) NSInteger step;
@property (nonatomic, readonly) NSUInteger count;

+ (id)rangeWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (id)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (ODClassType*)type;
- (id)applyIndex:(NSUInteger)index;
- (id<CNIterator>)iterator;
- (CNRange*)setStep:(NSInteger)step;
- (BOOL)isEmpty;
+ (CNRange*)applyI:(NSInteger)i;
+ (ODClassType*)type;
@end


@interface CNRangeIterator : NSObject<CNIterator>
@property (nonatomic, readonly) NSInteger start;
@property (nonatomic, readonly) NSInteger end;
@property (nonatomic, readonly) NSInteger step;

+ (id)rangeIteratorWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (id)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


