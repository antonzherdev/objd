#import "objd.h"
#import "CNMap.h"

#import "CNDispatchQueue.h"
#import "CNChain.h"
#import "CNPlat.h"
#import "ODType.h"
@implementation CNImMapDefault
static ODClassType* _CNImMapDefault_type;
@synthesize map = _map;
@synthesize defaultFunc = _defaultFunc;

+ (instancetype)imMapDefaultWithMap:(id<CNImMap>)map defaultFunc:(id(^)(id))defaultFunc {
    return [[CNImMapDefault alloc] initWithMap:map defaultFunc:defaultFunc];
}

- (instancetype)initWithMap:(id<CNImMap>)map defaultFunc:(id(^)(id))defaultFunc {
    self = [super init];
    if(self) {
        _map = map;
        _defaultFunc = [defaultFunc copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImMapDefault class]) _CNImMapDefault_type = [ODClassType classTypeWithCls:[CNImMapDefault class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [_map iterator];
}

- (id)applyKey:(id)key {
    id __tmp = [_map optKey:key];
    if(__tmp != nil) return ((id)(__tmp));
    else return _defaultFunc(key);
}

- (id<CNIterable>)keys {
    return [_map keys];
}

- (id<CNIterable>)values {
    return [_map values];
}

- (BOOL)containsKey:(id)key {
    return [_map containsKey:key];
}

- (BOOL)isEqualMap:(id<CNMap>)map {
    return [_map isEqual:map];
}

- (BOOL)isEqualMapDefault:(CNImMapDefault*)mapDefault {
    return [_map isEqual:mapDefault.map];
}

- (NSUInteger)hash {
    return [_map hash];
}

- (CNMMapDefault*)mCopy {
    return [CNMMapDefault mapDefaultWithMap:[_map mCopy] defaultFunc:_defaultFunc];
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
    return [CNImMapDefault type];
}

+ (ODClassType*)type {
    return _CNImMapDefault_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other)) return NO;
    if([other conformsToProtocol:@protocol(CNMap)]) return [self isEqualMap:((id<CNMap>)(other))];
    if([other isKindOfClass:[CNImMapDefault class]]) return [self isEqualMapDefault:((CNImMapDefault*)(other))];
    return NO;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"map=%@", self.map];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNMMapDefault
static ODClassType* _CNMMapDefault_type;
@synthesize map = _map;
@synthesize defaultFunc = _defaultFunc;

+ (instancetype)mapDefaultWithMap:(id<CNMMap>)map defaultFunc:(id(^)(id))defaultFunc {
    return [[CNMMapDefault alloc] initWithMap:map defaultFunc:defaultFunc];
}

- (instancetype)initWithMap:(id<CNMMap>)map defaultFunc:(id(^)(id))defaultFunc {
    self = [super init];
    if(self) {
        _map = map;
        _defaultFunc = [defaultFunc copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMMapDefault class]) _CNMMapDefault_type = [ODClassType classTypeWithCls:[CNMMapDefault class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [_map iterator];
}

- (id<CNMIterator>)mutableIterator {
    return [_map mutableIterator];
}

- (id)applyKey:(id)key {
    return [_map objectForKey:key orUpdateWith:^id() {
        return _defaultFunc(key);
    }];
}

- (id<CNIterable>)keys {
    return [_map keys];
}

- (id<CNIterable>)values {
    return [_map values];
}

- (BOOL)containsKey:(id)key {
    return [_map containsKey:key];
}

- (void)setKey:(id)key value:(id)value {
    [_map setKey:key value:value];
}

- (id)modifyKey:(id)key by:(id(^)(id))by {
    id value = by([self applyKey:key]);
    [_map setKey:key value:value];
    return value;
}

- (void)appendItem:(CNTuple*)item {
    [_map appendItem:item];
}

- (BOOL)removeItem:(CNTuple*)item {
    return [_map removeItem:item];
}

- (void)clear {
    [_map clear];
}

- (CNImMapDefault*)im {
    return [CNImMapDefault imMapDefaultWithMap:[_map im] defaultFunc:_defaultFunc];
}

- (CNImMapDefault*)imCopy {
    return [CNImMapDefault imMapDefaultWithMap:[_map imCopy] defaultFunc:_defaultFunc];
}

- (void)mutableFilterBy:(BOOL(^)(id))by {
    id<CNMIterator> i = [self mutableIterator];
    while([i hasNext]) {
        if(by([i next])) [i remove];
    }
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
    return [CNMMapDefault type];
}

+ (ODClassType*)type {
    return _CNMMapDefault_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"map=%@", self.map];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNHashMapBuilder
static ODClassType* _CNHashMapBuilder_type;

+ (instancetype)hashMapBuilder {
    return [[CNHashMapBuilder alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _map = [CNMHashMap hashMap];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNHashMapBuilder class]) _CNHashMapBuilder_type = [ODClassType classTypeWithCls:[CNHashMapBuilder class]];
}

- (void)appendItem:(CNTuple*)item {
    [_map setKey:((CNTuple*)(item)).a value:((CNTuple*)(item)).b];
}

- (CNImHashMap*)build {
    return [_map im];
}

- (void)appendAllItems:(id<CNTraversable>)items {
    [items forEach:^void(id _) {
        [self appendItem:_];
    }];
}

- (ODClassType*)type {
    return [CNHashMapBuilder type];
}

+ (ODClassType*)type {
    return _CNHashMapBuilder_type;
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


