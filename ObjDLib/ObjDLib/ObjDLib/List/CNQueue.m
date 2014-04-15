#import "objd.h"
#import "CNQueue.h"

#import "ODType.h"
#import "CNList.h"
@implementation CNImQueue
static CNImQueue* _CNImQueue_empty;
static ODClassType* _CNImQueue_type;
@synthesize in = _in;
@synthesize out = _out;

+ (instancetype)imQueueWithIn:(CNImList*)in out:(CNImList*)out {
    return [[CNImQueue alloc] initWithIn:in out:out];
}

- (instancetype)initWithIn:(CNImList*)in out:(CNImList*)out {
    self = [super init];
    if(self) {
        _in = in;
        _out = out;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImQueue class]) {
        _CNImQueue_type = [ODClassType classTypeWithCls:[CNImQueue class]];
        _CNImQueue_empty = [CNImQueue imQueueWithIn:[CNImList apply] out:[CNImList apply]];
    }
}

+ (CNImQueue*)apply {
    return _CNImQueue_empty;
}

- (id<CNIterator>)iterator {
    return [CNQueueIterator queueIteratorWithIn:_in out:_out];
}

- (BOOL)isEmpty {
    return [_in isEmpty] && [_out isEmpty];
}

- (NSUInteger)count {
    return [_in count] + [_out count];
}

- (CNImQueue*)addItem:(id)item {
    if([self isEmpty]) return [CNImQueue imQueueWithIn:[CNImList apply] out:[CNImList applyItem:item]];
    else return [CNImQueue imQueueWithIn:[CNImList applyItem:item tail:_in] out:_out];
}

- (CNImQueue*)enqueueItem:(id)item {
    if([self isEmpty]) return [CNImQueue imQueueWithIn:[CNImList apply] out:[CNImList applyItem:item]];
    else return [CNImQueue imQueueWithIn:[CNImList applyItem:item tail:_in] out:_out];
}

- (CNTuple*)dequeue {
    if(!([_out isEmpty])) {
        return ((CNTuple*)(tuple([_out head], [CNImQueue imQueueWithIn:_in out:[_out tail]])));
    } else {
        if([_in isEmpty]) {
            return ((CNTuple*)(tuple(nil, self)));
        } else {
            CNImList* rev = [_in reverse];
            return ((CNTuple*)(tuple([rev head], [CNImQueue imQueueWithIn:[CNImList apply] out:[rev tail]])));
        }
    }
}

- (ODClassType*)type {
    return [CNImQueue type];
}

+ (ODClassType*)type {
    return _CNImQueue_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"in=%@", self.in];
    [description appendFormat:@", out=%@", self.out];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNQueueIterator
static ODClassType* _CNQueueIterator_type;
@synthesize in = _in;
@synthesize out = _out;

+ (instancetype)queueIteratorWithIn:(CNImList*)in out:(CNImList*)out {
    return [[CNQueueIterator alloc] initWithIn:in out:out];
}

- (instancetype)initWithIn:(CNImList*)in out:(CNImList*)out {
    self = [super init];
    if(self) {
        _in = in;
        _out = out;
        _i = [_in iterator];
        _isIn = YES;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNQueueIterator class]) _CNQueueIterator_type = [ODClassType classTypeWithCls:[CNQueueIterator class]];
}

- (BOOL)hasNext {
    if([_i hasNext]) {
        return YES;
    } else {
        if(_isIn) {
            _isIn = NO;
            _i = [[_out reverse] iterator];
            return [_i hasNext];
        } else {
            return NO;
        }
    }
}

- (id)next {
    if(!([_i hasNext]) && _isIn) {
        _isIn = NO;
        _i = [[_out reverse] iterator];
    }
    return [_i next];
}

- (ODClassType*)type {
    return [CNQueueIterator type];
}

+ (ODClassType*)type {
    return _CNQueueIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"in=%@", self.in];
    [description appendFormat:@", out=%@", self.out];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNMQueue
static ODClassType* _CNMQueue_type;

+ (instancetype)queue {
    return [[CNMQueue alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) __queue = [CNImQueue apply];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMQueue class]) _CNMQueue_type = [ODClassType classTypeWithCls:[CNMQueue class]];
}

- (void)enqueueItem:(id)item {
    __queue = [__queue addItem:item];
}

- (id)dequeue {
    CNTuple* p = [__queue dequeue];
    __queue = p.b;
    return p.a;
}

- (NSUInteger)count {
    return [__queue count];
}

- (ODClassType*)type {
    return [CNMQueue type];
}

+ (ODClassType*)type {
    return _CNMQueue_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


