#import "objd.h"
#import "CNMap.h"

#import "CNType.h"
#import "CNTuple.h"
#import "CNDispatchQueue.h"
#import "CNObject.h"
#import "CNChain.h"
#import "CNPlat.h"
@implementation CNMap_impl

- (id)applyKey:(id)key {
    @throw @"Method apply is abstract";
}

- (id<CNIterable>)keys {
    @throw @"Method keys is abstract";
}

- (id<CNIterable>)values {
    @throw @"Method values is abstract";
}

- (BOOL)containsKey:(id)key {
    return [self applyKey:key] != nil;
}

- (BOOL)isValueEqualKey:(id)key value:(id)value {
    id __tmp;
    {
        id _ = [self applyKey:key];
        if(_ != nil) __tmp = numb([_ isEqual:value]);
        else __tmp = nil;
    }
    if(__tmp != nil) return unumb(__tmp);
    else return NO;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
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

@end

@implementation CNMMap_impl

- (void)appendItem:(CNTuple*)item {
    [self setKey:((CNTuple*)(item)).a value:((CNTuple*)(item)).b];
}

- (BOOL)removeItem:(CNTuple*)item {
    return [self removeKey:((CNTuple*)(item)).a] != nil;
}

- (id<CNImMap>)im {
    return [self imCopy];
}

- (id<CNImMap>)imCopy {
    CNMHashMap* arr = [CNMHashMap hashMap];
    {
        id<CNIterator> __il__1i = [self iterator];
        while([__il__1i hasNext]) {
            CNTuple* item = [__il__1i next];
            [arr setKey:((CNTuple*)(item)).a value:((CNTuple*)(item)).b];
        }
    }
    return [arr im];
}

- (void)setKey:(id)key value:(id)value {
    @throw @"Method set is abstract";
}

- (id)removeKey:(id)key {
    @throw @"Method remove is abstract";
}

- (id)applyKey:(id)key orUpdateWith:(id(^)())orUpdateWith {
    id __tmp = [self applyKey:key];
    if(__tmp != nil) {
        return ((id)(__tmp));
    } else {
        id init = orUpdateWith();
        [self setKey:key value:init];
        return init;
    }
}

- (id)modifyKey:(id)key by:(id(^)(id))by {
    id newObject = by([self applyKey:key]);
    if(newObject == nil) [self removeKey:key];
    else [self setKey:key value:newObject];
    return newObject;
}

- (void)assignImMap:(id<CNImMap>)imMap {
    [self clear];
    {
        id<CNIterator> __il__1i = [imMap iterator];
        while([__il__1i hasNext]) {
            CNTuple* _ = [__il__1i next];
            [self appendItem:_];
        }
    }
}

- (id<CNMIterator>)mutableIterator {
    @throw @"Method mutableIterator is abstract";
}

- (void)mutableFilterBy:(BOOL(^)(CNTuple*))by {
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

@end

@implementation CNImMapDefault
static CNClassType* _CNImMapDefault_type;
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
    if(self == [CNImMapDefault class]) _CNImMapDefault_type = [CNClassType classTypeWithCls:[CNImMapDefault class]];
}

- (NSUInteger)count {
    return [_map count];
}

- (id<CNIterator>)iterator {
    return [_map iterator];
}

- (id)applyKey:(id)key {
    id __tmp = [_map applyKey:key];
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

- (NSString*)description {
    return [NSString stringWithFormat:@"ImMapDefault(%@)", _map];
}

- (BOOL)isEqual:(id)to {
    if(self == to) return YES;
    if(to == nil) return NO;
    if([to conformsToProtocol:@protocol(CNMap)]) return [self isEqualMap:((id<CNMap>)(to))];
    if([to isKindOfClass:[CNImMapDefault class]]) return [self isEqualMapDefault:((CNImMapDefault*)(to))];
    return NO;
}

- (CNClassType*)type {
    return [CNImMapDefault type];
}

+ (CNClassType*)type {
    return _CNImMapDefault_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMMapDefault
static CNClassType* _CNMMapDefault_type;
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
    if(self == [CNMMapDefault class]) _CNMMapDefault_type = [CNClassType classTypeWithCls:[CNMMapDefault class]];
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
    return [_map applyKey:key orUpdateWith:^id() {
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

- (id)removeKey:(id)key {
    return [_map removeKey:key];
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

- (NSString*)description {
    return [NSString stringWithFormat:@"MMapDefault(%@)", _map];
}

- (CNClassType*)type {
    return [CNMMapDefault type];
}

+ (CNClassType*)type {
    return _CNMMapDefault_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNHashMapBuilder
static CNClassType* _CNHashMapBuilder_type;

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
    if(self == [CNHashMapBuilder class]) _CNHashMapBuilder_type = [CNClassType classTypeWithCls:[CNHashMapBuilder class]];
}

- (void)appendItem:(CNTuple*)item {
    [_map setKey:((CNTuple*)(item)).a value:((CNTuple*)(item)).b];
}

- (CNImHashMap*)build {
    return [_map im];
}

- (NSString*)description {
    return @"HashMapBuilder";
}

- (CNClassType*)type {
    return [CNHashMapBuilder type];
}

+ (CNClassType*)type {
    return _CNHashMapBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

