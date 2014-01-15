#import "objd.h"
#import "CNCollection.h"

#import "ODType.h"
#import "CNChain.h"
@implementation CNIterableF{
    id<CNIterator>(^_iteratorF)();
}
static ODClassType* _CNIterableF_type;
@synthesize iteratorF = _iteratorF;

+ (id)iterableFWithIteratorF:(id<CNIterator>(^)())iteratorF {
    return [[CNIterableF alloc] initWithIteratorF:iteratorF];
}

- (id)initWithIteratorF:(id<CNIterator>(^)())iteratorF {
    self = [super init];
    if(self) _iteratorF = iteratorF;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNIterableF_type = [ODClassType classTypeWithCls:[CNIterableF class]];
}

- (id<CNIterator>)iterator {
    return ((id<CNIterator>(^)())(_iteratorF))();
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
    return [[self iterator] next];
}

- (id)headOpt {
    if([self isEmpty]) return [CNOption none];
    else return [CNOption applyValue:[self head]];
}

- (BOOL)isEmpty {
    return !([[self iterator] hasNext]);
}

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
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

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = [CNOption none];
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = [CNOption applyValue:x];
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

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNIterableF* o = ((CNIterableF*)(other));
    return [self.iteratorF isEqual:o.iteratorF];
}

@end


@implementation CNEmptyIterator
static CNEmptyIterator* _CNEmptyIterator_instance;
static ODClassType* _CNEmptyIterator_type;

+ (id)emptyIterator {
    return [[CNEmptyIterator alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNEmptyIterator_type = [ODClassType classTypeWithCls:[CNEmptyIterator class]];
    _CNEmptyIterator_instance = [CNEmptyIterator emptyIterator];
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

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    return YES;
}

- (NSUInteger)hash {
    return 0;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


