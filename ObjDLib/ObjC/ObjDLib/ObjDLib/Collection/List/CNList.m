#import "objd.h"
#import "CNList.h"

#import "CNType.h"
@implementation CNImList
static CNClassType* _CNImList_type;

+ (instancetype)imList {
    return [[CNImList alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImList class]) _CNImList_type = [CNClassType classTypeWithCls:[CNImList class]];
}

+ (CNImList*)apply {
    return ((CNImList*)(CNEmptyList.instance));
}

+ (CNImList*)applyItem:(id)item {
    return [CNFilledList filledListWith_head:item tail:CNEmptyList.instance];
}

+ (CNImList*)applyItem:(id)item tail:(CNImList*)tail {
    return [CNFilledList filledListWith_head:item tail:tail];
}

- (id<CNIterator>)iterator {
    CNListIterator* i = [CNListIterator listIterator];
    i.list = self;
    return i;
}

- (CNImList*)tail {
    @throw @"Method tail is abstract";
}

- (CNImList*)filterF:(BOOL(^)(id))f {
    @throw @"Method filter is abstract";
}

- (CNImList*)reverse {
    @throw @"Method reverse is abstract";
}

- (CNImList*)insertItem:(id)item {
    @throw @"Method insert is abstract";
}

- (CNClassType*)type {
    return [CNImList type];
}

+ (CNClassType*)type {
    return _CNImList_type;
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


@implementation CNFilledList
static CNClassType* _CNFilledList_type;
@synthesize _head = __head;
@synthesize tail = _tail;
@synthesize count = _count;

+ (instancetype)filledListWith_head:(id)_head tail:(CNImList*)tail {
    return [[CNFilledList alloc] initWith_head:_head tail:tail];
}

- (instancetype)initWith_head:(id)_head tail:(CNImList*)tail {
    self = [super init];
    if(self) {
        __head = _head;
        _tail = tail;
        _count = [tail count] + 1;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFilledList class]) _CNFilledList_type = [CNClassType classTypeWithCls:[CNFilledList class]];
}

- (id)head {
    return __head;
}

- (BOOL)isEmpty {
    return NO;
}

- (CNImList*)filterF:(BOOL(^)(id))f {
    if(f(__head)) return ((CNImList*)([CNFilledList filledListWith_head:__head tail:[_tail filterF:f]]));
    else return [_tail filterF:f];
}

- (CNImList*)reverse {
    return [self reverseAndAddList:((CNImList*)(CNEmptyList.instance))];
}

- (CNImList*)reverseAndAddList:(CNImList*)list {
    CNFilledList* ret = [CNFilledList filledListWith_head:__head tail:list];
    CNImList* l = _tail;
    while(!([l isEmpty])) {
        ret = [CNFilledList filledListWith_head:((CNFilledList*)(l))._head tail:ret];
        l = [l tail];
    }
    return ret;
}

- (void)forEach:(void(^)(id))each {
    CNFilledList* list = self;
    while(YES) {
        each(list._head);
        CNImList* tail = list.tail;
        if([tail isEmpty]) return ;
        list = ((CNFilledList*)(tail));
    }
}

- (CNImList*)insertItem:(id)item {
    CNImList* before = [CNImList apply];
    CNFilledList* list = self;
    while(YES) {
        id h = list._head;
        if([item compareTo:((id)(h))] < 0) return [[CNFilledList filledListWith_head:((id)(item)) tail:before] reverseAndAddList:list];
        before = [CNImList applyItem:h tail:before];
        if([list.tail isEmpty]) return [[CNFilledList filledListWith_head:((id)(item)) tail:before] reverse];
        list = ((CNFilledList*)(list.tail));
    }
}

- (CNClassType*)type {
    return [CNFilledList type];
}

+ (CNClassType*)type {
    return _CNFilledList_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNFilledList* o = ((CNFilledList*)(other));
    return [self._head isEqual:o._head] && [self.tail isEqual:o.tail];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self._head hash];
    hash = hash * 31 + [self.tail hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"_head=%@", self._head];
    [description appendFormat:@", tail=%@", self.tail];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNEmptyList
static CNEmptyList* _CNEmptyList_instance;
static CNClassType* _CNEmptyList_type;

+ (instancetype)emptyList {
    return [[CNEmptyList alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNEmptyList class]) {
        _CNEmptyList_type = [CNClassType classTypeWithCls:[CNEmptyList class]];
        _CNEmptyList_instance = [CNEmptyList emptyList];
    }
}

- (NSUInteger)count {
    return 0;
}

- (id)head {
    return nil;
}

- (CNImList*)tail {
    return self;
}

- (BOOL)isEmpty {
    return YES;
}

- (CNImList*)filterF:(BOOL(^)(id))f {
    return self;
}

- (CNImList*)reverse {
    return self;
}

- (void)forEach:(void(^)(id))each {
}

- (CNImList*)insertItem:(id)item {
    return [CNImList applyItem:((id)(item))];
}

- (CNClassType*)type {
    return [CNEmptyList type];
}

+ (CNEmptyList*)instance {
    return _CNEmptyList_instance;
}

+ (CNClassType*)type {
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


@implementation CNListIterator
static CNClassType* _CNListIterator_type;
@synthesize list = _list;

+ (instancetype)listIterator {
    return [[CNListIterator alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _list = ((CNImList*)(CNEmptyList.instance));
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNListIterator class]) _CNListIterator_type = [CNClassType classTypeWithCls:[CNListIterator class]];
}

- (BOOL)hasNext {
    return !([_list isEmpty]);
}

- (id)next {
    id ret = [_list head];
    _list = [_list tail];
    return ((id)(ret));
}

- (CNClassType*)type {
    return [CNListIterator type];
}

+ (CNClassType*)type {
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


@implementation CNImListBuilder
static CNClassType* _CNImListBuilder_type;

+ (instancetype)imListBuilder {
    return [[CNImListBuilder alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _list = [CNImList apply];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImListBuilder class]) _CNImListBuilder_type = [CNClassType classTypeWithCls:[CNImListBuilder class]];
}

- (void)appendItem:(id)item {
    _list = [CNImList applyItem:item tail:_list];
}

- (CNImList*)build {
    return [_list reverse];
}

- (CNClassType*)type {
    return [CNImListBuilder type];
}

+ (CNClassType*)type {
    return _CNImListBuilder_type;
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


