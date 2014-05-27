#import "objd.h"
#import "CNDispatchQueue.h"

#import "CNType.h"
@implementation CNDispatchQueue{
    dispatch_queue_t _ref;
}
static CNDispatchQueue* _CNDispatchQueue_default;
static CNDispatchQueue* _CNDispatchQueue_mainThread;
static CNClassType * _CNDispatchQueue_type;
@synthesize ref = _ref;

+ (id)dispatchQueueWithRef:(dispatch_queue_t)ref {
    return [[CNDispatchQueue alloc] initWithRef:ref];
}

- (id)initWithRef:(dispatch_queue_t)ref {
    self = [super init];
    if(self) _ref = ref;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNDispatchQueue class]) {
        _CNDispatchQueue_type = [CNClassType classTypeWithCls:[CNDispatchQueue class]];
        _CNDispatchQueue_default = [CNDispatchQueue dispatchQueueWithRef:dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)];
        _CNDispatchQueue_mainThread = [CNDispatchQueue dispatchQueueWithRef:dispatch_get_main_queue()];
    }
}

- (void)asyncF:(void(^)())f {
    dispatch_async(_ref, f);
}

- (CNClassType *)type {
    return [CNDispatchQueue type];
}

+ (CNDispatchQueue*)aDefault {
    return _CNDispatchQueue_default;
}

+ (CNClassType *)type {
    return _CNDispatchQueue_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

+ (CNDispatchQueue *)mainThread {
    return _CNDispatchQueue_mainThread;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNDispatchQueue* o = ((CNDispatchQueue*)(other));
    return self.ref == o.ref;
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + self.ref.hash;
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"ref=%ld", (long)self.ref];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNThread {

}
+ (void)sleepPeriod:(CGFloat)d {
    [NSThread sleepForTimeInterval:d];
}
@end