#import "objd.h"
#import "CNCollection.h"

#import "ODType.h"
#import "CNDispatchQueue.h"
#import "CNChain.h"
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

- (id<CNMIterable>)mCopy {
    NSMutableArray* arr = [NSMutableArray mutableArray];
    [self forEach:^void(id item) {
        [arr appendItem:item];
    }];
    return arr;
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

- (id)head {
    if([self isEmpty]) return nil;
    else return [[self iterator] next];
}

- (BOOL)isEmpty {
    return !([[self iterator] hasNext]);
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

- (BOOL)containsItem:(id)item {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if([[i next] isEqual:i]) return YES;
    }
    return NO;
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

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
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


