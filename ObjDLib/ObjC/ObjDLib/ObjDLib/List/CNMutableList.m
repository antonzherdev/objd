#import "objd.h"
#import "CNMutableList.h"

#import "ODType.h"
@implementation CNMList
static ODClassType* _CNMList_type;

+ (instancetype)list {
    return [[CNMList alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) __count = 0;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMList class]) _CNMList_type = [ODClassType classTypeWithCls:[CNMList class]];
}

- (NSUInteger)count {
    return __count;
}

- (id<CNIterator>)iterator {
    CNMListImmutableIterator* i = [CNMListImmutableIterator listImmutableIterator];
    i.item = _headItem;
    return i;
}

- (id<CNMIterator>)mutableIterator {
    CNMListIterator* i = [CNMListIterator listIteratorWithList:self];
    i.item = _headItem;
    return i;
}

- (void)insertIndex:(NSUInteger)index item:(id)item {
    if(index == 0) {
        [self prependItem:item];
    } else {
        if(index >= __count) {
            [self appendItem:item];
        } else {
            CNMListItem* c = _headItem;
            NSUInteger i = index;
            while(c != nil && i > 0) {
                c = ((CNMListItem*)(c)).next;
            }
            if(c != nil) {
                CNMListItem* li = [CNMListItem listItemWithData:item];
                {
                    CNMListItem* __tmp_0_3_1 = ((CNMListItem*)(c)).next;
                    if(__tmp_0_3_1 != nil) ((CNMListItem*)(__tmp_0_3_1)).prev = li;
                }
                ((CNMListItem*)(c)).next = li;
            } else {
                [self appendItem:item];
            }
        }
    }
}

- (void)prependItem:(id)item {
    CNMListItem* i = [CNMListItem listItemWithData:item];
    if(_headItem == nil) {
        _headItem = i;
        _lastItem = i;
        __count = 1;
    } else {
        i.next = ((CNMListItem*)(_headItem));
        ((CNMListItem*)(_headItem)).prev = i;
        _headItem = i;
        __count++;
    }
}

- (void)appendItem:(id)item {
    CNMListItem* i = [CNMListItem listItemWithData:item];
    if(_lastItem == nil) {
        _headItem = i;
        _lastItem = i;
        __count = 1;
    } else {
        i.prev = ((CNMListItem*)(_lastItem));
        ((CNMListItem*)(_lastItem)).next = i;
        _lastItem = i;
        __count++;
    }
}

- (void)removeListItem:(CNMListItem*)listItem {
    if(_headItem != nil && [_headItem isEqual:listItem]) {
        _headItem = ((CNMListItem*)(_headItem)).next;
        ((CNMListItem*)(_headItem)).prev = nil;
    } else {
        if(_lastItem != nil && [_lastItem isEqual:listItem]) {
            _lastItem = ((CNMListItem*)(_lastItem)).prev;
            ((CNMListItem*)(_lastItem)).next = nil;
        } else {
            {
                CNMListItem* __tmp_0_0 = listItem.prev;
                if(__tmp_0_0 != nil) ((CNMListItem*)(__tmp_0_0)).next = listItem.next;
            }
            {
                CNMListItem* __tmp_0_1 = listItem.next;
                if(__tmp_0_1 != nil) ((CNMListItem*)(__tmp_0_1)).prev = listItem.prev;
            }
        }
    }
    __count--;
}

- (void)clear {
    _headItem = nil;
    _lastItem = nil;
}

- (void)removeHead {
    CNMListItem* _ = _headItem;
    if(_ != nil) [self removeListItem:_];
}

- (void)removeLast {
    CNMListItem* _ = _lastItem;
    if(_ != nil) [self removeListItem:_];
}

- (id)takeHead {
    CNMListItem* h = _headItem;
    if(h != nil) {
        id r = h.data;
        [self removeListItem:h];
        return r;
    } else {
        return nil;
    }
}

- (id)last {
    return ((CNMListItem*)(_lastItem)).data;
}

- (id)takeLast {
    CNMListItem* h = _lastItem;
    if(h != nil) {
        id r = h.data;
        [self removeListItem:h];
        return r;
    } else {
        return nil;
    }
}

- (void)forEach:(void(^)(id))each {
    CNMListItem* i = _headItem;
    while(i != nil) {
        each(((CNMListItem*)(i)).data);
        i = ((CNMListItem*)(i)).next;
    }
}

- (BOOL)goOn:(BOOL(^)(id))on {
    CNMListItem* i = _headItem;
    while(i != nil) {
        if(!(on(((CNMListItem*)(i)).data))) return NO;
        i = ((CNMListItem*)(i)).next;
    }
    return YES;
}

- (void)mutableFilterBy:(BOOL(^)(id))by {
    CNMListItem* i = _headItem;
    while(i != nil) {
        if(!(by(((CNMListItem*)(i)).data))) [self removeListItem:i];
        i = ((CNMListItem*)(i)).next;
    }
}

- (id)head {
    return ((CNMListItem*)(_headItem)).data;
}

- (ODClassType*)type {
    return [CNMList type];
}

+ (ODClassType*)type {
    return _CNMList_type;
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


@implementation CNMListItem
static ODClassType* _CNMListItem_type;
@synthesize data = _data;
@synthesize next = _next;
@synthesize prev = _prev;

+ (instancetype)listItemWithData:(id)data {
    return [[CNMListItem alloc] initWithData:data];
}

- (instancetype)initWithData:(id)data {
    self = [super init];
    if(self) _data = data;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMListItem class]) _CNMListItem_type = [ODClassType classTypeWithCls:[CNMListItem class]];
}

- (ODClassType*)type {
    return [CNMListItem type];
}

+ (ODClassType*)type {
    return _CNMListItem_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"data=%@", self.data];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNMListIterator
static ODClassType* _CNMListIterator_type;
@synthesize list = _list;
@synthesize item = _item;

+ (instancetype)listIteratorWithList:(CNMList*)list {
    return [[CNMListIterator alloc] initWithList:list];
}

- (instancetype)initWithList:(CNMList*)list {
    self = [super init];
    if(self) _list = list;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMListIterator class]) _CNMListIterator_type = [ODClassType classTypeWithCls:[CNMListIterator class]];
}

- (BOOL)hasNext {
    return _item != nil;
}

- (id)next {
    _prev = _item;
    _item = ((CNMListItem*)(nonnil(_item))).next;
    return ((CNMListItem*)(nonnil(_prev))).data;
}

- (void)remove {
    [_list removeListItem:((CNMListItem*)(nonnil(_prev)))];
}

- (void)setValue:(id)value {
    ((CNMListItem*)(nonnil(_prev))).data = value;
}

- (ODClassType*)type {
    return [CNMListIterator type];
}

+ (ODClassType*)type {
    return _CNMListIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"list=%@", self.list];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNMListImmutableIterator
static ODClassType* _CNMListImmutableIterator_type;
@synthesize item = _item;

+ (instancetype)listImmutableIterator {
    return [[CNMListImmutableIterator alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMListImmutableIterator class]) _CNMListImmutableIterator_type = [ODClassType classTypeWithCls:[CNMListImmutableIterator class]];
}

- (BOOL)hasNext {
    return _item != nil;
}

- (id)next {
    CNMListItem* r = _item;
    _item = ((CNMListItem*)(nonnil(_item))).next;
    return ((CNMListItem*)(nonnil(r))).data;
}

- (ODClassType*)type {
    return [CNMListImmutableIterator type];
}

+ (ODClassType*)type {
    return _CNMListImmutableIterator_type;
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


