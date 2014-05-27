#import "objdcore.h"
#import "CNObject.h"
#import "CNCollection.h"
@class CNClassType;
@class CNImList;
@class CNTuple;

@class CNQueue_impl;
@class CNImQueue;
@class CNQueueIterator;
@class CNMQueue;
@protocol CNQueue;

@protocol CNQueue<NSObject>
- (NSString*)description;
@end


@interface CNQueue_impl : NSObject<CNQueue>
+ (instancetype)queue_impl;
- (instancetype)init;
@end


@interface CNImQueue : CNQueue_impl {
@protected
    CNImList* _in;
    CNImList* _out;
}
@property (nonatomic, readonly) CNImList* in;
@property (nonatomic, readonly) CNImList* out;

+ (instancetype)imQueueWithIn:(CNImList*)in out:(CNImList*)out;
- (instancetype)initWithIn:(CNImList*)in out:(CNImList*)out;
- (CNClassType*)type;
+ (CNImQueue*)apply;
- (id<CNIterator>)iterator;
- (BOOL)isEmpty;
- (NSUInteger)count;
- (CNImQueue*)addItem:(id)item;
- (CNImQueue*)enqueueItem:(id)item;
- (CNTuple*)dequeue;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNQueueIterator : CNIterator_impl {
@protected
    CNImList* _out;
    id<CNIterator> _i;
    BOOL _isIn;
}
@property (nonatomic, readonly) CNImList* out;

+ (instancetype)queueIteratorWithIn:(CNImList*)in out:(CNImList*)out;
- (instancetype)initWithIn:(CNImList*)in out:(CNImList*)out;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNMQueue : CNQueue_impl {
@protected
    CNImQueue* __queue;
}
+ (instancetype)queue;
- (instancetype)init;
- (CNClassType*)type;
- (void)enqueueItem:(id)item;
- (id)dequeue;
- (NSUInteger)count;
- (NSString*)description;
+ (CNClassType*)type;
@end


