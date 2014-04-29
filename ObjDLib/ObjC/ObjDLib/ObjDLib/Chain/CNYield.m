#import "objd.h"
#import "CNYield.h"

#import "CNCollection.h"
#import "CNPlat.h"
#import "CNType.h"
@implementation CNYield
static CNClassType* _CNYield_type;
@synthesize begin = _begin;
@synthesize yield = _yield;
@synthesize end = _end;
@synthesize all = _all;

+ (instancetype)yieldWithBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [[CNYield alloc] initWithBegin:begin yield:yield end:end all:all];
}

- (instancetype)initWithBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
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

+ (char)Continue {
    return 0;
}

+ (char)Break {
    return 1;
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:yield end:end all:all];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end {
    return [CNYield makeBegin:begin yield:yield end:end all:nil];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:begin yield:yield end:nil all:all];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield {
    return [CNYield makeBegin:begin yield:yield end:nil all:nil];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:begin yield:nil end:end all:all];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end {
    return [CNYield makeBegin:begin yield:nil end:end all:nil];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin all:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:begin yield:nil end:nil all:all];
}

+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin {
    return [CNYield makeBegin:begin yield:nil end:nil all:nil];
}

+ (CNYield*)makeYield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:yield end:end all:all];
}

+ (CNYield*)makeYield:(int(^)(id))yield end:(int(^)(int))end {
    return [CNYield makeBegin:nil yield:yield end:end all:nil];
}

+ (CNYield*)makeYield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:yield end:nil all:all];
}

+ (CNYield*)makeYield:(int(^)(id))yield {
    return [CNYield makeBegin:nil yield:yield end:nil all:nil];
}

+ (CNYield*)makeEnd:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:nil end:end all:all];
}

+ (CNYield*)makeEnd:(int(^)(int))end {
    return [CNYield makeBegin:nil yield:nil end:end all:nil];
}

+ (CNYield*)makeAll:(int(^)(id<CNTraversable>))all {
    return [CNYield makeBegin:nil yield:nil end:nil all:all];
}

+ (CNYield*)make {
    return [CNYield makeBegin:nil yield:nil end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:({
        int(^__tmp)(NSUInteger) = begin;
        ((__tmp != nil) ? ((int(^)(NSUInteger))(__tmp)) : ^int(NSUInteger size) {
            return [base beginYieldWithSize:size];
        });
    }) yield:({
        int(^__tmp)(id) = yield;
        ((__tmp != nil) ? ((int(^)(id))(__tmp)) : ^int(id item) {
            return [base yieldItem:item];
        });
    }) end:({
        int(^__tmp)(int) = end;
        ((__tmp != nil) ? ((int(^)(int))(__tmp)) : ^int(int result) {
            return [base endYieldWithResult:result];
        });
    }) all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end {
    return [CNYield decorateBase:base begin:begin yield:yield end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:begin yield:yield end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield {
    return [CNYield decorateBase:base begin:begin yield:yield end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:begin yield:nil end:end all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin end:(int(^)(int))end {
    return [CNYield decorateBase:base begin:begin yield:nil end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:begin yield:nil end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin {
    return [CNYield decorateBase:base begin:begin yield:nil end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:yield end:end all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield end:(int(^)(int))end {
    return [CNYield decorateBase:base begin:nil yield:yield end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:yield end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield {
    return [CNYield decorateBase:base begin:nil yield:yield end:nil all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:nil end:end all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base end:(int(^)(int))end {
    return [CNYield decorateBase:base begin:nil yield:nil end:end all:nil];
}

+ (CNYield*)decorateBase:(CNYield*)base all:(int(^)(id<CNTraversable>))all {
    return [CNYield decorateBase:base begin:nil yield:nil end:nil all:all];
}

+ (CNYield*)decorateBase:(CNYield*)base {
    return [CNYield decorateBase:base begin:nil yield:nil end:nil all:nil];
}

- (int)beginYieldWithSize:(NSUInteger)size {
    if(_begin == nil) return ((int)(0));
    else return _begin(size);
}

- (int)yieldItem:(id)item {
    if(_yield == nil) return ((int)(0));
    else return _yield(item);
}

- (int)endYieldWithResult:(int)result {
    if(_end == nil) return result;
    else return _end(result);
}

- (int)yieldAllItems:(id<CNTraversable>)items {
    if(_all != nil) return _all(items);
    else return [self stdYieldAllItems:items];
}

- (int)stdYieldAllItems:(id<CNTraversable>)items {
    __block NSInteger result = ((NSInteger)(0));
    if([items isKindOfClass:[CNArray class]]) {
        CNArray* _items = ((CNArray*)(items));
        if([self beginYieldWithSize:[_items count]] == 1) return ((int)(1));
        else for(id item in _items) {
            result = ((NSInteger)([self yieldItem:item]));
            if(result == 0) continue;
            else break;
        }
    } else {
        if([items conformsToProtocol:@protocol(CNIterable)]) {
            id<CNIterable> _items = ((id<CNIterable>)(items));
            if([self beginYieldWithSize:[_items count]] == 1) return ((int)(1));
            else [items goOn:^BOOL(id item) {
                result = ((NSInteger)([self yieldItem:item]));
                return result == 0;
            }];
        } else {
            if([self beginYieldWithSize:0] == 1) return ((int)(1));
            else [items goOn:^BOOL(id item) {
                result = ((NSInteger)([self yieldItem:item]));
                return result == 0;
            }];
        }
    }
    return [self endYieldWithResult:((int)(result))];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end {
    return [CNYield yieldWithBegin:begin yield:yield end:end all:nil];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:yield end:nil all:all];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield {
    return [CNYield yieldWithBegin:begin yield:yield end:nil all:nil];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:nil end:end all:all];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end {
    return [CNYield yieldWithBegin:begin yield:nil end:end all:nil];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:begin yield:nil end:nil all:all];
}

+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin {
    return [CNYield yieldWithBegin:begin yield:nil end:nil all:nil];
}

+ (CNYield*)applyYield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:yield end:end all:all];
}

+ (CNYield*)applyYield:(int(^)(id))yield end:(int(^)(int))end {
    return [CNYield yieldWithBegin:nil yield:yield end:end all:nil];
}

+ (CNYield*)applyYield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:yield end:nil all:all];
}

+ (CNYield*)applyYield:(int(^)(id))yield {
    return [CNYield yieldWithBegin:nil yield:yield end:nil all:nil];
}

+ (CNYield*)applyEnd:(int(^)(int))end all:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:nil end:end all:all];
}

+ (CNYield*)applyEnd:(int(^)(int))end {
    return [CNYield yieldWithBegin:nil yield:nil end:end all:nil];
}

+ (CNYield*)applyAll:(int(^)(id<CNTraversable>))all {
    return [CNYield yieldWithBegin:nil yield:nil end:nil all:all];
}

+ (CNYield*)apply {
    return [CNYield yieldWithBegin:nil yield:nil end:nil all:nil];
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"begin=%@", self.begin];
    [description appendFormat:@", yield=%@", self.yield];
    [description appendFormat:@", end=%@", self.end];
    [description appendFormat:@", all=%@", self.all];
    [description appendString:@">"];
    return description;
}

@end


