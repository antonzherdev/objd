#import "objd.h"
#import "CNOption.h"

#import "ODType.h"
#import "CNSet.h"
#import "CNChain.h"
@implementation CNOption
static id _CNOption__none;
static ODClassType* _CNOption_type;

+ (id)option {
    return [[CNOption alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNOption_type = [ODClassType classTypeWithCls:[CNOption class]];
    _CNOption__none = [CNNone none];
}

+ (id)none {
    return _CNOption__none;
}

+ (id)applyValue:(id)value {
    if(value == nil) return _CNOption__none;
    else return [CNSome someWithValue:value];
}

+ (id)someValue:(id)value {
    if(value == nil) @throw @"Some with null";
    else return [CNSome someWithValue:value];
}

- (id)get {
    @throw @"Method get is abstract";
}

- (id)getOrElseF:(id(^)())f {
    @throw @"Method getOrElse is abstract";
}

- (id)getOrValue:(id)value {
    @throw @"Method getOr is abstract";
}

- (id)getOrNil {
    @throw @"Method getOrNil is abstract";
}

- (id)mapF:(id(^)(id))f {
    @throw @"Method map is abstract";
}

- (id)flatMapF:(id(^)(id))f {
    @throw @"Method flatMap is abstract";
}

- (id)filterF:(BOOL(^)(id))f {
    @throw @"Method filter is abstract";
}

- (BOOL)isEmpty {
    @throw @"Method isEmpty is abstract";
}

- (BOOL)isDefined {
    @throw @"Method isDefined is abstract";
}

- (void)forEach:(void(^)(id))each {
    @throw @"Method for is abstract";
}

- (BOOL)tryEach:(void(^)(id))each {
    @throw @"Method try is abstract";
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id)applyIndex:(NSUInteger)index {
    id<CNIterator> i = [self iterator];
    NSUInteger n = index;
    while([i hasNext]) {
        if(n == 0) return [i next];
        [i next];
        n--;
    }
    @throw @"Incorrect index";
}

- (id)optIndex:(NSUInteger)index {
    if(index >= [self count]) return [CNOption none];
    else return [CNOption applyValue:[self applyIndex:index]];
}

- (id)randomItem {
    NSUInteger c = [self count];
    if(c == 0) {
        return [CNOption none];
    } else {
        if(c == 1) return [CNOption applyValue:[self head]];
        else return [CNOption applyValue:[self applyIndex:oduIntRndMax([self count] - 1)]];
    }
}

- (id<CNSet>)toSet {
    return [self convertWithBuilder:[CNHashSetBuilder hashSetBuilder]];
}

- (id<CNSeq>)addItem:(id)item {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
}

- (id<CNSeq>)addSeq:(id<CNSeq>)seq {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendAllItems:seq];
    return [builder build];
}

- (id<CNSeq>)subItem:(id)item {
    return [[[self chain] filter:^BOOL(id _) {
        return !([_ isEqual:item]);
    }] toArray];
}

- (BOOL)isEqualToSeq:(id<CNSeq>)seq {
    if([self count] != [seq count]) return NO;
    id<CNIterator> ia = [self iterator];
    id<CNIterator> ib = [seq iterator];
    while([ia hasNext] && [ib hasNext]) {
        if(!([[ia next] isEqual:[ib next]])) return NO;
    }
    return YES;
}

- (id)head {
    return [self applyIndex:0];
}

- (id)headOpt {
    return [self optIndex:0];
}

- (id<CNSeq>)tail {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    id<CNIterator> i = [self iterator];
    if([i hasNext]) {
        [i next];
        while([i hasNext]) {
            [builder appendItem:[i next]];
        }
    }
    return [builder build];
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

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
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
    return [CNOption type];
}

+ (ODClassType*)type {
    return _CNOption_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other)) return NO;
    if([other conformsToProtocol:@protocol(CNSeq)]) return [self isEqualToSeq:((id<CNSeq>)(other))];
    return NO;
}

@end


@implementation CNNone
static ODClassType* _CNNone_type;

+ (id)none {
    return [[CNNone alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNNone_type = [ODClassType classTypeWithCls:[CNNone class]];
}

- (NSUInteger)count {
    return 0;
}

- (id)get {
    @throw @"Get from empty";
}

- (id)getOrElseF:(id(^)())f {
    return ((id(^)())(f))();
}

- (id)getOrValue:(id)value {
    return value;
}

- (id)getOrNil {
    return nil;
}

- (void)forEach:(void(^)(id))each {
}

- (id)mapF:(id(^)(id))f {
    return self;
}

- (id)flatMapF:(id(^)(id))f {
    return self;
}

- (id)filterF:(BOOL(^)(id))f {
    return self;
}

- (BOOL)isEmpty {
    return YES;
}

- (BOOL)isDefined {
    return NO;
}

- (id<CNIterator>)iterator {
    return CNEmptyIterator.instance;
}

- (BOOL)goOn:(BOOL(^)(id))on {
    return YES;
}

- (BOOL)tryEach:(void(^)(id))each {
    return NO;
}

- (BOOL)containsItem:(id)item {
    return NO;
}

- (ODClassType*)type {
    return [CNNone type];
}

+ (ODClassType*)type {
    return _CNNone_type;
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


@implementation CNSome{
    id _value;
}
static ODClassType* _CNSome_type;
@synthesize value = _value;

+ (id)someWithValue:(id)value {
    return [[CNSome alloc] initWithValue:value];
}

- (id)initWithValue:(id)value {
    self = [super init];
    if(self) _value = value;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNSome_type = [ODClassType classTypeWithCls:[CNSome class]];
}

- (NSUInteger)count {
    return 1;
}

- (id)get {
    return _value;
}

- (id)getOrElseF:(id(^)())f {
    return _value;
}

- (id)getOrNil {
    return _value;
}

- (id)getOrValue:(id)value {
    return _value;
}

- (id)mapF:(id(^)(id))f {
    return [CNSome someWithValue:f(_value)];
}

- (id)flatMapF:(id(^)(id))f {
    return f(_value);
}

- (id)filterF:(BOOL(^)(id))f {
    if(f(_value)) return self;
    else return [CNOption none];
}

- (BOOL)isEmpty {
    return NO;
}

- (BOOL)isDefined {
    return YES;
}

- (id<CNIterator>)iterator {
    return [CNSomeIterator someIteratorWithValue:_value];
}

- (void)forEach:(void(^)(id))each {
    each(_value);
}

- (BOOL)tryEach:(void(^)(id))each {
    each(_value);
    return YES;
}

- (BOOL)goOn:(BOOL(^)(id))on {
    return on(_value);
}

- (BOOL)containsItem:(id)item {
    return [_value isEqual:item];
}

- (ODClassType*)type {
    return [CNSome type];
}

+ (ODClassType*)type {
    return _CNSome_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNSome* o = ((CNSome*)(other));
    return [self.value isEqual:o.value];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.value hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"value=%@", self.value];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNSomeIterator{
    id _value;
    BOOL _hasNext;
}
static ODClassType* _CNSomeIterator_type;
@synthesize value = _value;
@synthesize hasNext = _hasNext;

+ (id)someIteratorWithValue:(id)value {
    return [[CNSomeIterator alloc] initWithValue:value];
}

- (id)initWithValue:(id)value {
    self = [super init];
    if(self) {
        _value = value;
        _hasNext = YES;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNSomeIterator_type = [ODClassType classTypeWithCls:[CNSomeIterator class]];
}

- (id)next {
    _hasNext = NO;
    return _value;
}

- (ODClassType*)type {
    return [CNSomeIterator type];
}

+ (ODClassType*)type {
    return _CNSomeIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNSomeIterator* o = ((CNSomeIterator*)(other));
    return [self.value isEqual:o.value];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.value hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"value=%@", self.value];
    [description appendString:@">"];
    return description;
}

@end


