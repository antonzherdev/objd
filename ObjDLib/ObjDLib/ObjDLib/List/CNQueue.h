#import "objdcore.h"
#import "CNSeq.h"
#import "ODObject.h"
#import "CNCollection.h"
@class CNList;
@class CNOption;
@class ODClassType;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNChain;

@class CNQueue;
@class CNQueueIterator;

@interface CNQueue : NSObject<CNSeq>
@property (nonatomic, readonly) CNList* in;
@property (nonatomic, readonly) CNList* out;

+ (id)queueWithIn:(CNList*)in out:(CNList*)out;
- (id)initWithIn:(CNList*)in out:(CNList*)out;
- (ODClassType*)type;
+ (CNQueue*)apply;
- (id<CNIterator>)iterator;
- (BOOL)isEmpty;
- (NSUInteger)count;
- (id)applyIndex:(NSUInteger)index;
- (CNQueue*)enqueueItem:(id)item;
- (CNTuple*)dequeue;
+ (ODClassType*)type;
@end


@interface CNQueueIterator : NSObject<CNIterator>
@property (nonatomic, readonly) CNList* in;
@property (nonatomic, readonly) CNList* out;

+ (id)queueIteratorWithIn:(CNList*)in out:(CNList*)out;
- (id)initWithIn:(CNList*)in out:(CNList*)out;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


