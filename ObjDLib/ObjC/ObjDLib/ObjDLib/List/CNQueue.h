#import "objdcore.h"
#import "ODObject.h"
#import "CNCollection.h"
@class CNImList;
@class ODClassType;

@class CNImQueue;
@class CNQueueIterator;
@class CNMQueue;
@protocol CNQueue;

@protocol CNQueue<NSObject>
@end


@interface CNImQueue : NSObject<CNQueue> {
@protected
    CNImList* _in;
    CNImList* _out;
}
@property (nonatomic, readonly) CNImList* in;
@property (nonatomic, readonly) CNImList* out;

+ (instancetype)imQueueWithIn:(CNImList*)in out:(CNImList*)out;
- (instancetype)initWithIn:(CNImList*)in out:(CNImList*)out;
- (ODClassType*)type;
+ (CNImQueue*)apply;
- (id<CNIterator>)iterator;
- (BOOL)isEmpty;
- (NSUInteger)count;
- (CNImQueue*)addItem:(id)item;
- (CNImQueue*)enqueueItem:(id)item;
- (CNTuple*)dequeue;
+ (ODClassType*)type;
@end


@interface CNQueueIterator : NSObject<CNIterator> {
@protected
    CNImList* _in;
    CNImList* _out;
    id<CNIterator> _i;
    BOOL _isIn;
}
@property (nonatomic, readonly) CNImList* in;
@property (nonatomic, readonly) CNImList* out;

+ (instancetype)queueIteratorWithIn:(CNImList*)in out:(CNImList*)out;
- (instancetype)initWithIn:(CNImList*)in out:(CNImList*)out;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNMQueue : NSObject<CNQueue> {
@protected
    CNImQueue* __queue;
}
+ (instancetype)queue;
- (instancetype)init;
- (ODClassType*)type;
- (void)enqueueItem:(id)item;
- (id)dequeue;
- (NSUInteger)count;
+ (ODClassType*)type;
@end


