#import "objd.h"
#import "CNMap.h"

#import "CNDispatchQueue.h"
#import "CNChain.h"
#import "CNPlat.h"
#import "ODType.h"
@implementation CNMap_impl

- (id)applyKey:(id)key {
    @throw @"Method apply is abstract";
}

- (id)optKey:(id)key {
    @throw @"Method opt is abstract";
}

- (id)getKey:(id)key orValue:(id)orValue {
    id __tmp = [self optKey:key];
    if(__tmp != nil) return ((id)(__tmp));
    else return orValue;
}

- (id<CNIterable>)keys {
    @throw @"Method keys is abstract";
}

- (id<CNIterable>)values {
    @throw @"Method values is abstract";
}

- (BOOL)containsKey:(id)key {
    return [self optKey:key] != nil;
}

- (BOOL)isValueEqualKey:(id)key value:(id)value {
    id __tmp;
    {
        id _ = [self optKey:key];
        if(_ != nil) __tmp = numb([_ isEqual:value]);
        else __tmp = nil;
    }
    if(__tmp != nil) return unumb(__tmp);
    else return NO;
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


@implementation CNImMap_impl

- (id<CNMMap>)mCopy {
    CNMHashMap* m = [CNMHashMap hashMap];
    [m assignImMap:self];
    return m;
}

- (id<CNImMap>)addItem:(CNTuple*)item {
    CNHashMapBuilder* builder = [CNHashMapBuilder hashMapBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
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


@implementation CNMMap_impl

- (void)appendItem:(CNTuple*)item {
    [self setKey:((CNTuple*)(item)).b value:((CNTuple*)(item)).a];
}

- (BOOL)removeItem:(CNTuple*)item {
    return [self removeForKey:((CNTuple*)(item)).a] != nil;
}

- (id<CNImMap>)im {
    return [self imCopy];
}

- (id<CNImMap>)imCopy {
    CNMHashMap* arr = [CNMHashMap hashMap];
    [self forEach:^void(CNTuple* item) {
        [arr setKey:((CNTuple*)(item)).a value:((CNTuple*)(item)).b];
    }];
    return [arr im];
}

- (void)setKey:(id)key value:(id)value {
    @throw @"Method set is abstract";
}

- (id)removeForKey:(id)key {
    @throw @"Method removeFor is abstract";
}

- (id)objectForKey:(id)key orUpdateWith:(id(^)())orUpdateWith {
    id __tmp = [self optKey:key];
    if(__tmp != nil) {
        return ((id)(__tmp));
    } else {
        id init = orUpdateWith();
        [self setKey:key value:init];
        return init;
    }
}

- (id)modifyKey:(id)key by:(id(^)(id))by {
    id newObject = by([self optKey:key]);
    if(newObject == nil) [self removeForKey:key];
    else [self setKey:key value:newObject];
    return newObject;
}

- (id)takeKey:(id)key {
    id ret = [self optKey:key];
    [self removeForKey:key];
    return ret;
}

- (void)assignImMap:(id<CNImMap>)imMap {
    [self clear];
    [imMap forEach:^void(CNTuple* _) {
        [self appendItem:_];
    }];
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


