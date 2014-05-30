#import "CNYield.h"

@implementation CNChainLink_impl

+ (instancetype)chainLink_impl {
    return [[CNChainLink_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (CNYield*)buildYield:(CNYield*)yield {
    @throw @"Method build is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNYield
static CNClassType* _CNYield_type;
@synthesize begin = _begin;
@synthesize yield = _yield;
@synthesize end = _end;
@synthesize all = _all;

+ (instancetype)yieldWithBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [[CNYield alloc] initWithBegin:begin yield:yield end:end all:all];
}

- (instancetype)initWithBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    self = [super init];
    if(self) {
        _begin = begin;
        _yield = yield;
        _end = end;
        _all = all;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNYield class]) _CNYield_type = [CNClassType classTypeWithCls:[CNYield class]];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:yield end:end all:all];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end {
    return [CNYield makeBegin:begin yield:yield end:end all:nil];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:begin yield:yield end:nil all:all];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield {
    return [CNYield makeBegin:begin yield:yield end:nil all:nil];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:begin yield:nil end:end all:all];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end {
    return [CNYield makeBegin:begin yield:nil end:end all:nil];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:begin yield:nil end:nil all:all];
}

+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin {
    return [CNYield makeBegin:begin yield:nil end:nil all:nil];
}

+ (CNYield*)makeYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:yield end:end all:all];
}

+ (CNYield*)makeYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end {
    return [CNYield makeBegin:nil yield:yield end:end all:nil];
}

+ (CNYield*)makeYield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:yield end:nil all:all];
}

+ (CNYield*)makeYield:(CNGoR(^)(id))yield {
    return [CNYield makeBegin:nil yield:yield end:nil all:nil];
}

+ (CNYield*)makeEnd:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:nil end:end all:all];
}

+ (CNYield*)makeEnd:(CNGoR(^)(CNGoR))end {
    return [CNYield makeBegin:nil yield:nil end:end all:nil];
}

+ (CNYield*)makeAll:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:nil end:nil all:all];
}

+ (CNYield*)make {
    return [CNYield makeBegin:nil yield:nil end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:((begin != nil) ? begin : ^CNGoR(NSUInteger size) {
        return [base beginYieldWithSize:size];
    }) yield:((yield != nil) ? yield : ^CNGoR(id item) {
        return [base yieldItem:((id)(item))];
    }) end:((end != nil) ? end : ^CNGoR(CNGoR result) {
        return [base endYieldWithResult:result];
    }) all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end {
    return [CNYield decorateBase:base begin:begin yield:yield end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:begin yield:yield end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield {
    return [CNYield decorateBase:base begin:begin yield:yield end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:begin yield:nil end:end all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end {
    return [CNYield decorateBase:base begin:begin yield:nil end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:begin yield:nil end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin {
    return [CNYield decorateBase:base begin:begin yield:nil end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:yield end:end all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end {
    return [CNYield decorateBase:base begin:nil yield:yield end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:yield end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield {
    return [CNYield decorateBase:base begin:nil yield:yield end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:nil end:end all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base end:(CNGoR(^)(CNGoR))end {
    return [CNYield decorateBase:base begin:nil yield:nil end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:nil end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base {
    return [CNYield decorateBase:base begin:nil yield:nil end:nil all:nil];
}

- (CNGoR)beginYieldWithSize:(NSUInteger)size {
    if(_begin == nil) return CNGo_Continue;
    else return _begin(size);
}

- (CNGoR)yieldItem:(id)item {
    if(_yield == nil) return CNGo_Continue;
    else return _yield(item);
}

- (CNGoR)endYieldWithResult:(CNGoR)result {
    if(_end == nil) return result;
    else return _end(result);
}

- (CNGoR)yieldAllItems:(id<CNTraversable>)items {
    if(_all != nil) return _all(self, items);
    else return [self stdYieldAllItems:items];
}

- (CNGoR)stdYieldAllItems:(id<CNTraversable>)items {
    CNGoR result = CNGo_Continue;
    if([items isKindOfClass:[CNArray class]]) {
        CNArray* _items = ((CNArray*)(items));
        if([self beginYieldWithSize:[_items count]] == CNGo_Continue) {
            result = CNGo_Continue;
            for(id item in _items) {
                if([self yieldItem:item] == CNGo_Continue) {
                    continue;
                } else {
                    result = CNGo_Break;
                    break;
                }
            }
        }
    } else {
        if([items conformsToProtocol:@protocol(CNHashMap)]) {
            id<CNIterable> _items = ((id<CNIterable>)(items));
            if([self beginYieldWithSize:[_items count]] == CNGo_Continue) [items goOn:^CNGoR(id item) {
                return [self yieldItem:item];
            }];
        } else {
            if([items conformsToProtocol:@protocol(CNIterable)]) {
                id<CNIterable> _items = ((id<CNIterable>)(items));
                if([self beginYieldWithSize:[_items count]] == CNGo_Continue) {
                    CNGoR __il__1fft_1tret = CNGo_Continue;
                    id<CNIterator> __il__1fft_1ti = [_items iterator];
                    while([__il__1fft_1ti hasNext]) {
                        if(({
                            id item = [__il__1fft_1ti next];
                            [self yieldItem:item];
                        }) == CNGo_Break) {
                            __il__1fft_1tret = CNGo_Break;
                            break;
                        }
                    }
                    result = __il__1fft_1tret;
                }
            } else {
                if([self beginYieldWithSize:0] == CNGo_Continue) [items goOn:^CNGoR(id item) {
                    return [self yieldItem:item];
                }];
            }
        }
    }
    return [self endYieldWithResult:result];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end {
    return [CNYield yieldWithBegin:begin yield:yield end:end all:nil];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:yield end:nil all:all];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield {
    return [CNYield yieldWithBegin:begin yield:yield end:nil all:nil];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:nil end:end all:all];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end {
    return [CNYield yieldWithBegin:begin yield:nil end:end all:nil];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:nil end:nil all:all];
}

+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin {
    return [CNYield yieldWithBegin:begin yield:nil end:nil all:nil];
}

+ (CNYield*)applyYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:yield end:end all:all];
}

+ (CNYield*)applyYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end {
    return [CNYield yieldWithBegin:nil yield:yield end:end all:nil];
}

+ (CNYield*)applyYield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:yield end:nil all:all];
}

+ (CNYield*)applyYield:(CNGoR(^)(id))yield {
    return [CNYield yieldWithBegin:nil yield:yield end:nil all:nil];
}

+ (CNYield*)applyEnd:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:nil end:end all:all];
}

+ (CNYield*)applyEnd:(CNGoR(^)(CNGoR))end {
    return [CNYield yieldWithBegin:nil yield:nil end:end all:nil];
}

+ (CNYield*)applyAll:(CNGoR(^)(CNYield*, id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:nil end:nil all:all];
}

+ (CNYield*)apply {
    return [CNYield yieldWithBegin:nil yield:nil end:nil all:nil];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Yield(%@, %@, %@, %@)", _begin, _yield, _end, _all];
}

- (CNClassType*)type {
    return [CNYield type];
}

+ (CNClassType*)type {
    return _CNYield_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

