#import "CNObserver.h"

#import "CNAtomic.h"
#import "CNChain.h"
@implementation CNObservable_impl

+ (instancetype)observable_impl {
    return [[CNObservable_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (void)attachObserver:(CNObserver*)observer {
    @throw @"Method attach is abstract";
}

- (void)detachObserver:(CNObserver*)observer {
    @throw @"Method detach is abstract";
}

- (CNObserver*)observeF:(void(^)(id))f {
    CNObserver* obs = [CNObserver observerWithObservable:self f:f];
    [self attachObserver:obs];
    return obs;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNObservableBase_impl

+ (instancetype)observableBase_impl {
    return [[CNObservableBase_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) __observers = [CNAtomicObject atomicObjectWithValue:((NSArray*)((@[])))];
    
    return self;
}

- (void)attachObserver:(CNObserver*)observer {
    while(YES) {
        NSArray* v = [__observers value];
        if([__observers compareAndSetOldValue:v newValue:[v addItem:[CNWeak weakWithValue:observer]]]) return ;
    }
}

- (void)detachObserver:(CNObserver*)observer {
    BOOL(^p)(CNWeak*) = ((observer == nil) ? ^BOOL(CNWeak* l) {
        return !([l isEmpty]);
    } : ^BOOL(CNWeak* l) {
        CNObserver* lv = l->_value;
        return lv != observer && lv != nil;
    });
    while(YES) {
        NSArray* v = [__observers value];
        NSArray* nv = [[[v chain] filterWhen:p] toArray];
        if([__observers compareAndSetOldValue:v newValue:nv]) return ;
    }
}

- (void)notifyValue:(id)value {
    [((NSArray*)([__observers value])) forEach:^void(CNWeak* o) {
        CNObserver* v = o->_value;
        {
            void(^__nd)(id) = ((CNObserver*)(v)).f;
            if(__nd != nil) __nd(value);
        }
    }];
}

- (BOOL)hasObservers {
    return !([((NSArray*)([__observers value])) isEmpty]);
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNObserver
static CNClassType* _CNObserver_type;
@synthesize observable = _observable;
@synthesize f = _f;

+ (instancetype)observerWithObservable:(id<CNObservable>)observable f:(void(^)(id))f {
    return [[CNObserver alloc] initWithObservable:observable f:f];
}

- (instancetype)initWithObservable:(id<CNObservable>)observable f:(void(^)(id))f {
    self = [super init];
    if(self) {
        _observable = observable;
        _f = [f copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNObserver class]) _CNObserver_type = [CNClassType classTypeWithCls:[CNObserver class]];
}

- (void)detach {
    [_observable detachObserver:self];
}

- (void)dealloc {
    [_observable detachObserver:nil];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Observer(%@)", _observable];
}

- (CNClassType*)type {
    return [CNObserver type];
}

+ (CNClassType*)type {
    return _CNObserver_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNSignal
static CNClassType* _CNSignal_type;

+ (instancetype)signal {
    return [[CNSignal alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSignal class]) _CNSignal_type = [CNClassType classTypeWithCls:[CNSignal class]];
}

- (void)postData:(id)data {
    [self notifyValue:data];
}

- (void)post {
    [((CNSignal*)(self)) notifyValue:nil];
}

- (NSString*)description {
    return @"Signal";
}

- (CNClassType*)type {
    return [CNSignal type];
}

+ (CNClassType*)type {
    return _CNSignal_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

