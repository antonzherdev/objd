#import "objd.h"
#import "CNMap.h"

#import "ODType.h"
#import "CNChain.h"
@implementation CNMapDefault{
    id(^_defaultFunc)(id);
    id<CNMutableMap> _map;
}
static ODClassType* _CNMapDefault_type;
@synthesize defaultFunc = _defaultFunc;
@synthesize map = _map;

+ (id)mapDefaultWithDefaultFunc:(id(^)(id))defaultFunc map:(id<CNMutableMap>)map {
    return [[CNMapDefault alloc] initWithDefaultFunc:defaultFunc map:map];
}

- (id)initWithDefaultFunc:(id(^)(id))defaultFunc map:(id<CNMutableMap>)map {
    self = [super init];
    if(self) {
        _defaultFunc = defaultFunc;
        _map = map;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNMapDefault_type = [ODClassType classTypeWithCls:[CNMapDefault class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [_map iterator];
}

- (id<CNMutableIterator>)mutableIterator {
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

- (id)modifyBy:(id(^)(id))by forKey:(id)forKey {
    id value = by([self applyKey:forKey]);
    [_map setKey:forKey value:value];
    return value;
}

- (void)appendItem:(CNTuple*)item {
    [_map appendItem:item];
}

- (void)removeItem:(CNTuple*)item {
    [_map removeItem:item];
}

- (void)clear {
    [_map clear];
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
    return [CNMapDefault type];
}

+ (ODClassType*)type {
    return _CNMapDefault_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNMapDefault* o = ((CNMapDefault*)(other));
    return [self.defaultFunc isEqual:o.defaultFunc] && [self.map isEqual:o.map];
}

@end


@implementation CNHashMapBuilder{
    NSMutableDictionary* _map;
}
static ODClassType* _CNHashMapBuilder_type;
@synthesize map = _map;

+ (id)hashMapBuilder {
    return [[CNHashMapBuilder alloc] init];
}

- (id)init {
    self = [super init];
    if(self) _map = [NSMutableDictionary mutableDictionary];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNHashMapBuilder_type = [ODClassType classTypeWithCls:[CNHashMapBuilder class]];
}

- (void)appendItem:(CNTuple*)item {
    [_map setKey:item.a value:item.b];
}

- (NSDictionary*)build {
    return _map;
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


