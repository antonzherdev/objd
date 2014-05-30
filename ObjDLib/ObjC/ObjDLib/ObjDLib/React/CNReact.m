#import "CNReact.h"

#import "CNDispatchQueue.h"
#import "CNAtomic.h"
#import "CNChain.h"
@implementation CNReact
static CNClassType* _CNReact_type;

+ (instancetype)react {
    return [[CNReact alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNReact class]) _CNReact_type = [CNClassType classTypeWithCls:[CNReact class]];
}

+ (CNReact*)applyValue:(id)value {
    return [CNVal valWithValue:value];
}

+ (CNReact*)applyA:(CNReact*)a f:(id(^)(id))f {
    return [CNMappedReact mappedReactWithA:a f:f];
}

+ (CNReact*)applyA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    return [CNMappedReact2 mappedReact2WithA:a b:b f:f];
}

+ (CNReact*)applyA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    return [CNMappedReact3 mappedReact3WithA:a b:b c:c f:f];
}

+ (CNReact*)asyncQueue:(CNDispatchQueue*)queue a:(CNReact*)a f:(id(^)(id))f {
    return [CNAsyncMappedReact asyncMappedReactWithQueue:queue a:a f:f];
}

+ (CNReact*)asyncA:(CNReact*)a f:(id(^)(id))f {
    return [CNReact asyncQueue:CNDispatchQueue.aDefault a:a f:f];
}

+ (CNReact*)asyncQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    return [CNAsyncMappedReact2 asyncMappedReact2WithQueue:queue a:a b:b f:f];
}

+ (CNReact*)asyncA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    return [CNReact asyncQueue:CNDispatchQueue.aDefault a:a b:b f:f];
}

+ (CNReact*)asyncQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    return [CNAsyncMappedReact3 asyncMappedReact3WithQueue:queue a:a b:b c:c f:f];
}

+ (CNReact*)asyncA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    return [CNReact asyncQueue:CNDispatchQueue.aDefault a:a b:b c:c f:f];
}

- (void)attachObserver:(CNObserver*)observer {
    @throw @"Method attach is abstract";
}

- (void)detachObserver:(CNObserver*)observer {
    @throw @"Method detach is abstract";
}

- (id)value {
    @throw @"Method value is abstract";
}

- (CNReact*)mapF:(id(^)(id))f {
    return [CNMappedReact mappedReactWithA:self f:f];
}

- (CNReact*)flatMapF:(CNReact*(^)(id))f {
    return [CNFlatMappedReact flatMappedReactWithA:self f:f];
}

- (CNReact*)asyncMapQueue:(CNDispatchQueue*)queue f:(id(^)(id))f {
    return [CNAsyncMappedReact asyncMappedReactWithQueue:queue a:self f:f];
}

- (CNReact*)asyncMapF:(id(^)(id))f {
    return [self asyncMapQueue:CNDispatchQueue.aDefault f:f];
}

- (NSString*)description {
    return @"React";
}

- (CNClassType*)type {
    return [CNReact type];
}

+ (CNClassType*)type {
    return _CNReact_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNImReact
static CNClassType* _CNImReact_type;

+ (instancetype)imReact {
    return [[CNImReact alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImReact class]) _CNImReact_type = [CNClassType classTypeWithCls:[CNImReact class]];
}

- (void)attachObserver:(CNObserver*)observer {
}

- (void)detachObserver:(CNObserver*)observer {
}

- (NSString*)description {
    return @"ImReact";
}

- (CNClassType*)type {
    return [CNImReact type];
}

+ (CNClassType*)type {
    return _CNImReact_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMReact
static CNClassType* _CNMReact_type;
@synthesize _value = __value;

+ (instancetype)reactWithInitial:(id)initial {
    return [[CNMReact alloc] initWithInitial:initial];
}

- (instancetype)initWithInitial:(id)initial {
    self = [super init];
    if(self) {
        __value = [CNAtomicObject atomicObjectWithValue:initial];
        __observers = [CNAtomicObject atomicObjectWithValue:((NSArray*)((@[])))];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMReact class]) _CNMReact_type = [CNClassType classTypeWithCls:[CNMReact class]];
}

- (id)value {
    return [__value value];
}

- (void)_setValue:(id)value {
    while(YES) {
        id v = [__value value];
        if([v isEqual:value]) return ;
        if([__value compareAndSetOldValue:v newValue:value]) {
            [self notifyValue:value];
            return ;
        }
    }
}

- (NSString*)description {
    return @"MReact";
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
        CNObserver* lv = l.value;
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
        CNObserver* v = o.value;
        {
            void(^__nd)(id) = ((CNObserver*)(v)).f;
            if(__nd != nil) __nd(value);
        }
    }];
}

- (BOOL)hasObservers {
    return !([((NSArray*)([__observers value])) isEmpty]);
}

- (CNClassType*)type {
    return [CNMReact type];
}

+ (CNClassType*)type {
    return _CNMReact_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNVal
static CNClassType* _CNVal_type;
@synthesize value = _value;

+ (instancetype)valWithValue:(id)value {
    return [[CNVal alloc] initWithValue:value];
}

- (instancetype)initWithValue:(id)value {
    self = [super init];
    if(self) _value = value;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNVal class]) _CNVal_type = [CNClassType classTypeWithCls:[CNVal class]];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Val(%@)", _value];
}

- (BOOL)isEqual:(id)to {
    if(self == to) return YES;
    if(to == nil || !([to isKindOfClass:[CNVal class]])) return NO;
    CNVal* o = ((CNVal*)(to));
    return [_value isEqual:o.value];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [_value hash];
    return hash;
}

- (CNClassType*)type {
    return [CNVal type];
}

+ (CNClassType*)type {
    return _CNVal_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNVar
static CNClassType* _CNVar_type;

+ (instancetype)varWithInitial:(id)initial {
    return [[CNVar alloc] initWithInitial:initial];
}

- (instancetype)initWithInitial:(id)initial {
    self = [super initWithInitial:initial];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNVar class]) _CNVar_type = [CNClassType classTypeWithCls:[CNVar class]];
}

+ (CNVar*)applyInitial:(id)initial {
    return [CNSimpleVar simpleVarWithInitial:initial];
}

+ (CNVar*)limitedInitial:(id)initial limits:(id(^)(id))limits {
    return [CNLimitedVar limitedVarWithInitial:initial limits:limits];
}

+ (CNVar*)feedbackInitial:(id)initial feedback:(void(^)(id))feedback {
    return [CNFeedbackVar feedbackVarWithInitial:initial feedback:feedback];
}

- (void)setValue:(id)value {
    [self _setValue:value];
}

- (void)updateF:(id(^)(id))f {
    while(YES) {
        id v = [self._value value];
        id value = f(v);
        if([v isEqual:value]) return ;
        if([self._value compareAndSetOldValue:v newValue:value]) {
            [self notifyValue:value];
            return ;
        }
    }
}

- (NSString*)description {
    return @"Var";
}

- (CNClassType*)type {
    return [CNVar type];
}

+ (CNClassType*)type {
    return _CNVar_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNSimpleVar
static CNClassType* _CNSimpleVar_type;

+ (instancetype)simpleVarWithInitial:(id)initial {
    return [[CNSimpleVar alloc] initWithInitial:initial];
}

- (instancetype)initWithInitial:(id)initial {
    self = [super initWithInitial:initial];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSimpleVar class]) _CNSimpleVar_type = [CNClassType classTypeWithCls:[CNSimpleVar class]];
}

- (NSString*)description {
    return @"SimpleVar";
}

- (CNClassType*)type {
    return [CNSimpleVar type];
}

+ (CNClassType*)type {
    return _CNSimpleVar_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNFeedbackVar
static CNClassType* _CNFeedbackVar_type;
@synthesize feedback = _feedback;

+ (instancetype)feedbackVarWithInitial:(id)initial feedback:(void(^)(id))feedback {
    return [[CNFeedbackVar alloc] initWithInitial:initial feedback:feedback];
}

- (instancetype)initWithInitial:(id)initial feedback:(void(^)(id))feedback {
    self = [super initWithInitial:initial];
    if(self) _feedback = [feedback copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFeedbackVar class]) _CNFeedbackVar_type = [CNClassType classTypeWithCls:[CNFeedbackVar class]];
}

- (void)setValue:(id)value {
    [self _setValue:value];
    _feedback(value);
}

- (void)feedValue:(id)value {
    [self _setValue:value];
}

- (void)updateF:(id(^)(id))f {
    while(YES) {
        id v = [self._value value];
        id value = f(v);
        if([v isEqual:value]) return ;
        if([self._value compareAndSetOldValue:v newValue:value]) {
            [self notifyValue:value];
            _feedback(value);
            return ;
        }
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@")"];
}

- (CNClassType*)type {
    return [CNFeedbackVar type];
}

+ (CNClassType*)type {
    return _CNFeedbackVar_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNLimitedVar
static CNClassType* _CNLimitedVar_type;
@synthesize limits = _limits;

+ (instancetype)limitedVarWithInitial:(id)initial limits:(id(^)(id))limits {
    return [[CNLimitedVar alloc] initWithInitial:initial limits:limits];
}

- (instancetype)initWithInitial:(id)initial limits:(id(^)(id))limits {
    self = [super initWithInitial:limits(initial)];
    if(self) _limits = [limits copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNLimitedVar class]) _CNLimitedVar_type = [CNClassType classTypeWithCls:[CNLimitedVar class]];
}

- (void)setValue:(id)value {
    [self _setValue:_limits(value)];
}

- (void)updateF:(id(^)(id))f {
    while(YES) {
        id v = [self._value value];
        id value = _limits(f(v));
        if([v isEqual:value]) return ;
        if([self._value compareAndSetOldValue:v newValue:value]) {
            [self notifyValue:value];
            return ;
        }
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@")"];
}

- (CNClassType*)type {
    return [CNLimitedVar type];
}

+ (CNClassType*)type {
    return _CNLimitedVar_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNSlot
static CNClassType* _CNSlot_type;

+ (instancetype)slotWithInitial:(id)initial {
    return [[CNSlot alloc] initWithInitial:initial];
}

- (instancetype)initWithInitial:(id)initial {
    self = [super initWithInitial:initial];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSlot class]) _CNSlot_type = [CNClassType classTypeWithCls:[CNSlot class]];
}

- (void)connectTo:(CNReact*)to {
    __weak CNSlot* _weakSelf = self;
    @synchronized(self) {
        __base = to;
        if(__observer != nil) [((CNObserver*)(__observer)) detach];
        __observer = [to observeF:^void(id newValue) {
            CNSlot* _self = _weakSelf;
            if(_self != nil) [_self _setValue:newValue];
        }];
        [self _setValue:[to value]];
    }
}

- (void)setValue:(id)value {
    [self connectTo:[CNVal valWithValue:value]];
}

- (NSString*)description {
    return @"Slot";
}

- (CNClassType*)type {
    return [CNSlot type];
}

+ (CNClassType*)type {
    return _CNSlot_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNReactExpression
static CNClassType* _CNReactExpression_type;

+ (instancetype)reactExpressionWithInitial:(id)initial {
    return [[CNReactExpression alloc] initWithInitial:initial];
}

- (instancetype)initWithInitial:(id)initial {
    self = [super initWithInitial:initial];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNReactExpression class]) _CNReactExpression_type = [CNClassType classTypeWithCls:[CNReactExpression class]];
}

- (void)recalc {
    [self _setValue:[self calc]];
}

- (id)calc {
    @throw @"Method calc is abstract";
}

- (NSString*)description {
    return @"ReactExpression";
}

- (CNClassType*)type {
    return [CNReactExpression type];
}

+ (CNClassType*)type {
    return _CNReactExpression_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMappedReact
static CNClassType* _CNMappedReact_type;
@synthesize a = _a;
@synthesize f = _f;

+ (instancetype)mappedReactWithA:(CNReact*)a f:(id(^)(id))f {
    return [[CNMappedReact alloc] initWithA:a f:f];
}

- (instancetype)initWithA:(CNReact*)a f:(id(^)(id))f {
    self = [super initWithInitial:f([a value])];
    __weak CNMappedReact* _weakSelf = self;
    if(self) {
        _a = a;
        _f = [f copy];
        _obsA = [a observeF:^void(id newValue) {
            CNMappedReact* _self = _weakSelf;
            if(_self != nil) [_self _setValue:f(newValue)];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMappedReact class]) _CNMappedReact_type = [CNClassType classTypeWithCls:[CNMappedReact class]];
}

- (id)calc {
    return _f([_a value]);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"MappedReact(%@)", _a];
}

- (CNClassType*)type {
    return [CNMappedReact type];
}

+ (CNClassType*)type {
    return _CNMappedReact_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMappedReact2
static CNClassType* _CNMappedReact2_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize f = _f;

+ (instancetype)mappedReact2WithA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    return [[CNMappedReact2 alloc] initWithA:a b:b f:f];
}

- (instancetype)initWithA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    self = [super initWithInitial:f([a value], [b value])];
    __weak CNMappedReact2* _weakSelf = self;
    if(self) {
        _a = a;
        _b = b;
        _f = [f copy];
        _obsA = [a observeF:^void(id newValue) {
            CNMappedReact2* _self = _weakSelf;
            if(_self != nil) [_self _setValue:f(newValue, [b value])];
        }];
        _obsB = [b observeF:^void(id newValue) {
            CNMappedReact2* _self = _weakSelf;
            if(_self != nil) [_self _setValue:f([a value], newValue)];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMappedReact2 class]) _CNMappedReact2_type = [CNClassType classTypeWithCls:[CNMappedReact2 class]];
}

- (id)calc {
    return _f([_a value], [_b value]);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"MappedReact2(%@, %@)", _a, _b];
}

- (CNClassType*)type {
    return [CNMappedReact2 type];
}

+ (CNClassType*)type {
    return _CNMappedReact2_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMappedReact3
static CNClassType* _CNMappedReact3_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;
@synthesize f = _f;

+ (instancetype)mappedReact3WithA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    return [[CNMappedReact3 alloc] initWithA:a b:b c:c f:f];
}

- (instancetype)initWithA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    self = [super initWithInitial:f([a value], [b value], [c value])];
    __weak CNMappedReact3* _weakSelf = self;
    if(self) {
        _a = a;
        _b = b;
        _c = c;
        _f = [f copy];
        _obsA = [a observeF:^void(id newValue) {
            CNMappedReact3* _self = _weakSelf;
            if(_self != nil) [_self _setValue:f(newValue, [b value], [c value])];
        }];
        _obsB = [b observeF:^void(id newValue) {
            CNMappedReact3* _self = _weakSelf;
            if(_self != nil) [_self _setValue:f([a value], newValue, [c value])];
        }];
        _obsC = [c observeF:^void(id newValue) {
            CNMappedReact3* _self = _weakSelf;
            if(_self != nil) [_self _setValue:f([a value], [b value], newValue)];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMappedReact3 class]) _CNMappedReact3_type = [CNClassType classTypeWithCls:[CNMappedReact3 class]];
}

- (id)calc {
    return _f([_a value], [_b value], [_c value]);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"MappedReact3(%@, %@, %@)", _a, _b, _c];
}

- (CNClassType*)type {
    return [CNMappedReact3 type];
}

+ (CNClassType*)type {
    return _CNMappedReact3_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNFlatMappedReact
static CNClassType* _CNFlatMappedReact_type;
@synthesize a = _a;
@synthesize f = _f;

+ (instancetype)flatMappedReactWithA:(CNReact*)a f:(CNReact*(^)(id))f {
    return [[CNFlatMappedReact alloc] initWithA:a f:f];
}

- (instancetype)initWithA:(CNReact*)a f:(CNReact*(^)(id))f {
    self = [super initWithInitial:[f([a value]) value]];
    __weak CNFlatMappedReact* _weakSelf = self;
    if(self) {
        _a = a;
        _f = [f copy];
        _obsA = [a observeF:^void(id newValue) {
            CNFlatMappedReact* _self = _weakSelf;
            if(_self != nil) [_self _setValue:[f(newValue) value]];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFlatMappedReact class]) _CNFlatMappedReact_type = [CNClassType classTypeWithCls:[CNFlatMappedReact class]];
}

- (id)calc {
    return [_f([_a value]) value];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"FlatMappedReact(%@)", _a];
}

- (CNClassType*)type {
    return [CNFlatMappedReact type];
}

+ (CNClassType*)type {
    return _CNFlatMappedReact_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNAsyncMappedReact
static CNClassType* _CNAsyncMappedReact_type;
@synthesize queue = _queue;
@synthesize a = _a;
@synthesize f = _f;

+ (instancetype)asyncMappedReactWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a f:(id(^)(id))f {
    return [[CNAsyncMappedReact alloc] initWithQueue:queue a:a f:f];
}

- (instancetype)initWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a f:(id(^)(id))f {
    self = [super initWithInitial:f([a value])];
    __weak CNAsyncMappedReact* _weakSelf = self;
    if(self) {
        _queue = queue;
        _a = a;
        _f = [f copy];
        _obsA = [a observeF:^void(id _) {
            [queue asyncF:^void() {
                CNAsyncMappedReact* _self = _weakSelf;
                if(_self != nil) [_self recalc];
            }];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNAsyncMappedReact class]) _CNAsyncMappedReact_type = [CNClassType classTypeWithCls:[CNAsyncMappedReact class]];
}

- (id)calc {
    return _f([_a value]);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"AsyncMappedReact(%@, %@)", _queue, _a];
}

- (CNClassType*)type {
    return [CNAsyncMappedReact type];
}

+ (CNClassType*)type {
    return _CNAsyncMappedReact_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNAsyncMappedReact2
static CNClassType* _CNAsyncMappedReact2_type;
@synthesize queue = _queue;
@synthesize a = _a;
@synthesize b = _b;
@synthesize f = _f;

+ (instancetype)asyncMappedReact2WithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    return [[CNAsyncMappedReact2 alloc] initWithQueue:queue a:a b:b f:f];
}

- (instancetype)initWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f {
    self = [super initWithInitial:f([a value], [b value])];
    __weak CNAsyncMappedReact2* _weakSelf = self;
    if(self) {
        _queue = queue;
        _a = a;
        _b = b;
        _f = [f copy];
        _obsA = [a observeF:^void(id _) {
            [queue asyncF:^void() {
                CNAsyncMappedReact2* _self = _weakSelf;
                if(_self != nil) [_self recalc];
            }];
        }];
        _obsB = [b observeF:^void(id _) {
            [queue asyncF:^void() {
                CNAsyncMappedReact2* _self = _weakSelf;
                if(_self != nil) [_self recalc];
            }];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNAsyncMappedReact2 class]) _CNAsyncMappedReact2_type = [CNClassType classTypeWithCls:[CNAsyncMappedReact2 class]];
}

- (id)calc {
    return _f([_a value], [_b value]);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"AsyncMappedReact2(%@, %@, %@)", _queue, _a, _b];
}

- (CNClassType*)type {
    return [CNAsyncMappedReact2 type];
}

+ (CNClassType*)type {
    return _CNAsyncMappedReact2_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNAsyncMappedReact3
static CNClassType* _CNAsyncMappedReact3_type;
@synthesize queue = _queue;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;
@synthesize f = _f;

+ (instancetype)asyncMappedReact3WithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    return [[CNAsyncMappedReact3 alloc] initWithQueue:queue a:a b:b c:c f:f];
}

- (instancetype)initWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f {
    self = [super initWithInitial:f([a value], [b value], [c value])];
    __weak CNAsyncMappedReact3* _weakSelf = self;
    if(self) {
        _queue = queue;
        _a = a;
        _b = b;
        _c = c;
        _f = [f copy];
        _obsA = [a observeF:^void(id _) {
            [queue asyncF:^void() {
                CNAsyncMappedReact3* _self = _weakSelf;
                if(_self != nil) [_self recalc];
            }];
        }];
        _obsB = [b observeF:^void(id _) {
            [queue asyncF:^void() {
                CNAsyncMappedReact3* _self = _weakSelf;
                if(_self != nil) [_self recalc];
            }];
        }];
        _obsC = [c observeF:^void(id _) {
            [queue asyncF:^void() {
                CNAsyncMappedReact3* _self = _weakSelf;
                if(_self != nil) [_self recalc];
            }];
        }];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNAsyncMappedReact3 class]) _CNAsyncMappedReact3_type = [CNClassType classTypeWithCls:[CNAsyncMappedReact3 class]];
}

- (id)calc {
    return _f([_a value], [_b value], [_c value]);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"AsyncMappedReact3(%@, %@, %@, %@)", _queue, _a, _b, _c];
}

- (CNClassType*)type {
    return [CNAsyncMappedReact3 type];
}

+ (CNClassType*)type {
    return _CNAsyncMappedReact3_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNReactFlag
static CNClassType* _CNReactFlag_type;
@synthesize reacts = _reacts;

+ (instancetype)reactFlagWithInitial:(BOOL)initial reacts:(NSArray*)reacts {
    return [[CNReactFlag alloc] initWithInitial:initial reacts:reacts];
}

- (instancetype)initWithInitial:(BOOL)initial reacts:(NSArray*)reacts {
    self = [super initWithInitial:numb(initial)];
    __weak CNReactFlag* _weakSelf = self;
    if(self) {
        _reacts = reacts;
        _observers = [[[reacts chain] mapF:^CNObserver*(id<CNObservable> r) {
            return [((id<CNObservable>)(r)) observeF:^void(id _) {
                CNReactFlag* _self = _weakSelf;
                if(_self != nil) [_self _setValue:@YES];
            }];
        }] toArray];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNReactFlag class]) _CNReactFlag_type = [CNClassType classTypeWithCls:[CNReactFlag class]];
}

- (void)set {
    [self _setValue:@YES];
}

- (void)setValue:(BOOL)value {
    [self _setValue:numb(value)];
}

- (void)clear {
    [self _setValue:@NO];
}

- (void)processF:(void(^)())f {
    if(unumb([self value])) {
        f();
        [self clear];
    }
}

+ (CNReactFlag*)applyInitial:(BOOL)initial {
    return [CNReactFlag reactFlagWithInitial:initial reacts:((NSArray*)((@[])))];
}

+ (CNReactFlag*)applyReacts:(NSArray*)reacts {
    return [CNReactFlag reactFlagWithInitial:YES reacts:reacts];
}

+ (CNReactFlag*)apply {
    return [CNReactFlag reactFlagWithInitial:YES reacts:((NSArray*)((@[])))];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"ReactFlag(%@)", _reacts];
}

- (CNClassType*)type {
    return [CNReactFlag type];
}

+ (CNClassType*)type {
    return _CNReactFlag_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

