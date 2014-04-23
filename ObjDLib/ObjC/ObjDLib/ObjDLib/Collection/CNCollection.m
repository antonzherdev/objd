#import "objd.h"
#import "CNCollection.h"

#import "CNDispatchQueue.h"
#import "CNChain.h"
#import "CNPlat.h"
#import "ODType.h"
@implementation CNIterator_impl

- (BOOL)hasNext {
    @throw @"Method hasNext is abstract";
}

- (id)next {
    @throw @"Method next is abstract";
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


@implementation CNMIterator_impl

- (void)remove {
    @throw @"Method remove is abstract";
}

- (void)setValue:(id)value {
    @throw @"Method set is abstract";
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


@implementation CNBuilder_impl

- (void)appendItem:(id)item {
    @throw @"Method append is abstract";
}

- (id)build {
    @throw @"Method build is abstract";
}

- (void)appendAllItems:(id<CNTraversable>)items {
    [items forEach:^void(id _) {
        [self appendItem:_];
    }];
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


@implementation CNTraversable_impl

- (void)forEach:(void(^)(id))each {
    [self goOn:^BOOL(id item) {
        each(item);
        return YES;
    }];
}

- (void)parForEach:(void(^)(id))each {
    [self goOn:^BOOL(id item) {
        [CNDispatchQueue.aDefault asyncF:^void() {
            each(item);
        }];
        return YES;
    }];
}

- (BOOL)goOn:(BOOL(^)(id))on {
    @throw @"Method go is abstract";
}

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = nil;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = x;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = YES;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    __block BOOL ret = YES;
    [self goOn:^BOOL(id x) {
        if(!(confirm(x))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (id)head {
    __block id ret;
    [self goOn:^BOOL(id on) {
        ret = on;
        return NO;
    }];
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
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


@implementation CNImTraversable_impl

- (id<CNMTraversable>)mCopy {
    CNMArray* arr = [CNMArray array];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
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


@implementation CNMTraversable_impl

- (void)appendItem:(id)item {
    @throw @"Method append is abstract";
}

- (BOOL)removeItem:(id)item {
    @throw @"Method remove is abstract";
}

- (void)clear {
    @throw @"Method clear is abstract";
}

- (id<CNImTraversable>)im {
    return [self imCopy];
}

- (id<CNImTraversable>)imCopy {
    CNMArray* arr = [CNMArray array];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
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


@implementation CNIterable_impl

- (id)head {
    if([self isEmpty]) return nil;
    else return [[self iterator] next];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
    }
}

- (void)parForEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        id v = [i next];
        [CNDispatchQueue.aDefault asyncF:^void() {
            each(v);
        }];
    }
}

- (BOOL)goOn:(BOOL(^)(id))on {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if(!(on([i next]))) return NO;
    }
    return YES;
}

- (NSString*)description {
    return [[self chain] toStringWithStart:@"[" delimiter:@", " end:@"]"];
}

- (NSUInteger)hash {
    NSUInteger ret = 13;
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        ret = ret * 31 + [[i next] hash];
    }
    return ret;
}

- (NSUInteger)count {
    id<CNIterator> i = [self iterator];
    NSUInteger n = 0;
    while([i hasNext]) {
        [i next];
        n++;
    }
    return n;
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (BOOL)isEmpty {
    return !([[self iterator] hasNext]);
}

- (BOOL)containsItem:(id)item {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if([[i next] isEqual:i]) return YES;
    }
    return NO;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end


@implementation CNImIterable_impl

- (id<CNMIterable>)mCopy {
    CNMArray* arr = [CNMArray array];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
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


@implementation CNMIterable_impl

- (BOOL)removeItem:(id)item {
    id<CNMIterator> i = [self mutableIterator];
    BOOL ret = NO;
    while([i hasNext]) {
        if([[i next] isEqual:item]) {
            [i remove];
            ret = YES;
        }
    }
    return ret;
}

- (id<CNImIterable>)im {
    return [self imCopy];
}

- (id<CNImIterable>)imCopy {
    CNMArray* arr = [CNMArray array];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
}

- (id<CNMIterator>)mutableIterator {
    @throw @"Method mutableIterator is abstract";
}

- (void)mutableFilterBy:(BOOL(^)(id))by {
    id<CNMIterator> i = [self mutableIterator];
    while([i hasNext]) {
        if(by([i next])) [i remove];
    }
}

- (void)appendItem:(id)item {
    @throw @"Method append is abstract";
}

- (void)clear {
    @throw @"Method clear is abstract";
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


@implementation CNIterableF
static ODClassType* _CNIterableF_type;
@synthesize iteratorF = _iteratorF;

+ (instancetype)iterableFWithIteratorF:(id<CNIterator>(^)())iteratorF {
    return [[CNIterableF alloc] initWithIteratorF:iteratorF];
}

- (instancetype)initWithIteratorF:(id<CNIterator>(^)())iteratorF {
    self = [super init];
    if(self) _iteratorF = [iteratorF copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNIterableF class]) _CNIterableF_type = [ODClassType classTypeWithCls:[CNIterableF class]];
}

- (id<CNIterator>)iterator {
    return _iteratorF();
}

- (ODClassType*)type {
    return [CNIterableF type];
}

+ (ODClassType*)type {
    return _CNIterableF_type;
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


@implementation CNEmptyIterator
static CNEmptyIterator* _CNEmptyIterator_instance;
static ODClassType* _CNEmptyIterator_type;

+ (instancetype)emptyIterator {
    return [[CNEmptyIterator alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNEmptyIterator class]) {
        _CNEmptyIterator_type = [ODClassType classTypeWithCls:[CNEmptyIterator class]];
        _CNEmptyIterator_instance = [CNEmptyIterator emptyIterator];
    }
}

- (BOOL)hasNext {
    return NO;
}

- (id)next {
    @throw @"Iterator is empty";
}

- (ODClassType*)type {
    return [CNEmptyIterator type];
}

+ (CNEmptyIterator*)instance {
    return _CNEmptyIterator_instance;
}

+ (ODClassType*)type {
    return _CNEmptyIterator_type;
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


