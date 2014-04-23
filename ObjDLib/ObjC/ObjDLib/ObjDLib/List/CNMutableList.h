#import "objdcore.h"
#import "CNSeq.h"
#import "ODObject.h"
#import "CNCollection.h"
@class ODClassType;

@class CNMList;
@class CNMListItem;
@class CNMListIterator;
@class CNMListImmutableIterator;

@interface CNMList : CNMSeq_impl {
@protected
    NSUInteger __count;
    CNMListItem* _headItem;
    CNMListItem* _lastItem;
}
+ (instancetype)list;
- (instancetype)init;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNMIterator>)mutableIterator;
- (void)insertIndex:(NSUInteger)index item:(id)item;
- (void)prependItem:(id)item;
- (void)appendItem:(id)item;
- (void)removeListItem:(CNMListItem*)listItem;
- (void)clear;
- (void)removeHead;
- (void)removeLast;
- (id)takeHead;
- (id)last;
- (id)takeLast;
- (void)forEach:(void(^)(id))each;
- (BOOL)goOn:(BOOL(^)(id))on;
- (void)mutableFilterBy:(BOOL(^)(id))by;
- (id)head;
+ (ODClassType*)type;
@end


@interface CNMListItem : NSObject {
@protected
    id _data;
    CNMListItem* _next;
    __weak CNMListItem* _prev;
}
@property (nonatomic, retain) id data;
@property (nonatomic) CNMListItem* next;
@property (nonatomic, weak) CNMListItem* prev;

+ (instancetype)listItemWithData:(id)data;
- (instancetype)initWithData:(id)data;
- (ODClassType*)type;
+ (ODClassType*)type;
@end


@interface CNMListIterator : CNMIterator_impl {
@protected
    CNMList* _list;
    CNMListItem* _prev;
    CNMListItem* _item;
}
@property (nonatomic, readonly) CNMList* list;
@property (nonatomic) CNMListItem* item;

+ (instancetype)listIteratorWithList:(CNMList*)list;
- (instancetype)initWithList:(CNMList*)list;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (void)remove;
- (void)setValue:(id)value;
+ (ODClassType*)type;
@end


@interface CNMListImmutableIterator : CNIterator_impl {
@protected
    __weak CNMListItem* _item;
}
@property (nonatomic, weak) CNMListItem* item;

+ (instancetype)listImmutableIterator;
- (instancetype)init;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


