#import "CNConcurrentQueue.h"

#import "CNLock.h"
#import "CNAtomic.h"
@implementation CNConcurrentQueueNode
static CNClassType* _CNConcurrentQueueNode_type;
@synthesize item = _item;
@synthesize next = _next;

+ (instancetype)concurrentQueueNode {
    return [[CNConcurrentQueueNode alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNConcurrentQueueNode class]) _CNConcurrentQueueNode_type = [CNClassType classTypeWithCls:[CNConcurrentQueueNode class]];
}

+ (CNConcurrentQueueNode*)applyItem:(id)item {
    CNConcurrentQueueNode* ret = [CNConcurrentQueueNode concurrentQueueNode];
    ret.item = item;
    return ret;
}

- (NSString*)description {
    return @"ConcurrentQueueNode";
}

- (CNClassType*)type {
    return [CNConcurrentQueueNode type];
}

+ (CNClassType*)type {
    return _CNConcurrentQueueNode_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNConcurrentQueue
static CNClassType* _CNConcurrentQueue_type;

+ (instancetype)concurrentQueue {
    return [[CNConcurrentQueue alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) {
        __head = [CNConcurrentQueueNode concurrentQueueNode];
        __tail = __head;
        _hLock = [NSLock lock];
        _tLock = [NSLock lock];
        __count = [CNAtomicInt atomicInt];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNConcurrentQueue class]) _CNConcurrentQueue_type = [CNClassType classTypeWithCls:[CNConcurrentQueue class]];
}

- (int)count {
    return [__count intValue];
}

- (void)enqueueItem:(id)item {
    CNConcurrentQueueNode* node = [CNConcurrentQueueNode applyItem:item];
    [_tLock lock];
    __tail.next = node;
    __tail = node;
    [__count incrementAndGet];
    [_tLock unlock];
}

- (id)dequeue {
    [_hLock lock];
    id ret;
    {
        CNConcurrentQueueNode* newHead = __head.next;
        if(newHead != nil) {
            id item = ((CNConcurrentQueueNode*)(newHead)).item;
            ((CNConcurrentQueueNode*)(newHead)).item = nil;
            __head = newHead;
            [__count decrementAndGet];
            ret = item;
        } else {
            ret = nil;
        }
    }
    [_hLock unlock];
    return ret;
}

- (id)dequeueWhen:(BOOL(^)(id))when {
    [_hLock lock];
    id ret;
    {
        CNConcurrentQueueNode* newHead = __head.next;
        if(newHead != nil) {
            id item = ((CNConcurrentQueueNode*)(newHead)).item;
            if(when(((id)(item)))) {
                ((CNConcurrentQueueNode*)(newHead)).item = nil;
                __head = newHead;
                [__count decrementAndGet];
                ret = item;
            } else {
                ret = nil;
            }
        } else {
            ret = nil;
        }
    }
    [_hLock unlock];
    return ret;
}

- (void)clear {
    [_hLock lock];
    __head = __tail;
    __head.item = nil;
    [__count setNewValue:0];
    [_hLock unlock];
}

- (id)peek {
    [_hLock lock];
    CNConcurrentQueueNode* node = __head;
    CNConcurrentQueueNode* newHead = node.next;
    if(newHead == nil) {
        [_hLock unlock];
        return nil;
    }
    id item = ((CNConcurrentQueueNode*)(newHead)).item;
    [_hLock unlock];
    return item;
}

- (BOOL)isEmpty {
    return [__count intValue] == 0;
}

- (NSString*)description {
    return @"ConcurrentQueue";
}

- (CNClassType*)type {
    return [CNConcurrentQueue type];
}

+ (CNClassType*)type {
    return _CNConcurrentQueue_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

