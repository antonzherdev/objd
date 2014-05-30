#import "objd.h"
@class NSLock;
@class CNAtomicInt;

@class CNConcurrentQueueNode;
@class CNConcurrentQueue;

@interface CNConcurrentQueueNode : NSObject {
@protected
    id _item;
    CNConcurrentQueueNode* _next;
}
@property (nonatomic) id item;
@property (nonatomic) CNConcurrentQueueNode* next;

+ (instancetype)concurrentQueueNode;
- (instancetype)init;
- (CNClassType*)type;
+ (CNConcurrentQueueNode*)applyItem:(id)item;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNConcurrentQueue : CNQueue_impl {
@protected
    CNConcurrentQueueNode* __head;
    CNConcurrentQueueNode* __tail;
    NSLock* _hLock;
    NSLock* _tLock;
    CNAtomicInt* __count;
}
+ (instancetype)concurrentQueue;
- (instancetype)init;
- (CNClassType*)type;
- (int)count;
- (void)enqueueItem:(id)item;
- (id)dequeue;
- (id)dequeueWhen:(BOOL(^)(id))when;
- (void)clear;
- (id)peek;
- (BOOL)isEmpty;
- (NSString*)description;
+ (CNClassType*)type;
@end


