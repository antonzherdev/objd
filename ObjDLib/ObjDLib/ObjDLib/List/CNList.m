#import "objd.h"
#import "CNList.h"

#import "ODType.h"
#import "CNSet.h"
#import "CNChain.h"
@implementation CNList
static ODClassType* _CNList_type;

+ (id)list {
    return [[CNList alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNList_type = [ODClassType classTypeWithCls:[CNList class]];
}

+ (CNList*)apply {
    return ((CNList*)(CNEmptyList.instance));
}

+ (CNList*)applyItem:(id)item {
    return [CNFilledList filledListWithHead:item tail:CNEmptyList.instance];
}

+ (CNList*)applyItem:(id)item tail:(CNList*)tail {
    return [CNFilledList filledListWithHead:item tail:tail];
}

- (id<CNIterator>)iterator {
    CNListIterator* i = [CNListIterator listIterator];
    i.list = self;
    return i;
}

- (CNList*)tail {
    @throw @"Method tail is abstract";
}

- (CNList*)filterF:(BOOL(^)(id))f {
    @throw @"Method filter is abstract";
}

- (CNList*)reverse {
    @throw @"Method reverse is abstract";
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

- (BOOL)isEmpty {
    return [self count] == 0;
}

- (id)head {
    return [self applyIndex:0];
}

- (id)headOpt {
    return [self optIndex:0];
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
    return [CNList type];
}

+ (ODClassType*)type {
    return _CNList_type;
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


@implementation CNFilledList{
    id _head;
    CNList* _tail;
    NSUInteger _count;
}
static ODClassType* _CNFilledList_type;
@synthesize head = _head;
@synthesize tail = _tail;
@synthesize count = _count;

+ (id)filledListWithHead:(id)head tail:(CNList*)tail {
    return [[CNFilledList alloc] initWithHead:head tail:tail];
}

- (id)initWithHead:(id)head tail:(CNList*)tail {
    self = [super init];
    if(self) {
        _head = head;
        _tail = tail;
        _count = [_tail count] + 1;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNFilledList_type = [ODClassType classTypeWithCls:[CNFilledList class]];
}

- (id)headOpt {
    return [CNOption applyValue:_head];
}

- (BOOL)isEmpty {
    return NO;
}

- (CNList*)filterF:(BOOL(^)(id))f {
    if(f(_head)) return [CNFilledList filledListWithHead:_head tail:[_tail filterF:f]];
    else return [_tail filterF:f];
}

- (CNList*)reverse {
    CNFilledList* ret = [CNFilledList filledListWithHead:_head tail:CNEmptyList.instance];
    CNList* list = _tail;
    while(!([list isEmpty])) {
        ret = [CNFilledList filledListWithHead:((CNFilledList*)(list)).head tail:ret];
        list = [list tail];
    }
    return ret;
}

- (void)forEach:(void(^)(id))each {
    CNFilledList* list = self;
    while(YES) {
        each(list.head);
        CNList* tail = list.tail;
        if([tail isEmpty]) return ;
        list = ((CNFilledList*)(tail));
    }
}

- (ODClassType*)type {
    return [CNFilledList type];
}

+ (ODClassType*)type {
    return _CNFilledList_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNFilledList* o = ((CNFilledList*)(other));
    return [self.head isEqual:o.head] && [self.tail isEqual:o.tail];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.head hash];
    hash = hash * 31 + [self.tail hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"head=%@", self.head];
    [description appendFormat:@", tail=%@", self.tail];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNEmptyList
static CNEmptyList* _CNEmptyList_instance;
static ODClassType* _CNEmptyList_type;

+ (id)emptyList {
    return [[CNEmptyList alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNEmptyList_type = [ODClassType classTypeWithCls:[CNEmptyList class]];
    _CNEmptyList_instance = [CNEmptyList emptyList];
}

- (NSUInteger)count {
    return 0;
}

- (id)head {
    @throw @"List is empty";
}

- (id)headOpt {
    return [CNOption none];
}

- (CNList*)tail {
    return self;
}

- (BOOL)isEmpty {
    return YES;
}

- (CNList*)filterF:(BOOL(^)(id))f {
    return self;
}

- (CNList*)reverse {
    return self;
}

- (void)forEach:(void(^)(id))each {
}

- (ODClassType*)type {
    return [CNEmptyList type];
}

+ (CNEmptyList*)instance {
    return _CNEmptyList_instance;
}

+ (ODClassType*)type {
    return _CNEmptyList_type;
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


@implementation CNListIterator{
    CNList* _list;
}
static ODClassType* _CNListIterator_type;
@synthesize list = _list;

+ (id)listIterator {
    return [[CNListIterator alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNListIterator_type = [ODClassType classTypeWithCls:[CNListIterator class]];
}

- (BOOL)hasNext {
    return !([_list isEmpty]);
}

- (id)next {
    id ret = [_list head];
    _list = [_list tail];
    return ret;
}

- (ODClassType*)type {
    return [CNListIterator type];
}

+ (ODClassType*)type {
    return _CNListIterator_type;
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


