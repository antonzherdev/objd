#import "objdcore.h"
#import "CNSeq.h"
#import "CNCollection.h"
@class CNClassType;

@class CNImList;
@class CNFilledList;
@class CNEmptyList;
@class CNListIterator;
@class CNImListBuilder;

@interface CNImList : CNImSeq_impl
+ (instancetype)imList;
- (instancetype)init;
- (CNClassType*)type;
+ (CNImList*)apply;
+ (CNImList*)applyItem:(id)item;
+ (CNImList*)applyItem:(id)item tail:(CNImList*)tail;
- (id<CNIterator>)iterator;
- (CNImList*)tail;
- (CNImList*)filterF:(BOOL(^)(id))f;
- (CNImList*)reverse;
- (CNImList*)insertItem:(id)item;
+ (CNClassType*)type;
@end


@interface CNFilledList : CNImList {
@protected
    id __head;
    CNImList* _tail;
    NSUInteger _count;
}
@property (nonatomic, readonly) id _head;
@property (nonatomic, readonly) CNImList* tail;
@property (nonatomic, readonly) NSUInteger count;

+ (instancetype)filledListWith_head:(id)_head tail:(CNImList*)tail;
- (instancetype)initWith_head:(id)_head tail:(CNImList*)tail;
- (CNClassType*)type;
- (id)head;
- (BOOL)isEmpty;
- (CNImList*)filterF:(BOOL(^)(id))f;
- (CNImList*)reverse;
- (void)forEach:(void(^)(id))each;
- (CNImList*)insertItem:(id)item;
+ (CNClassType*)type;
@end


@interface CNEmptyList : CNImList
+ (instancetype)emptyList;
- (instancetype)init;
- (CNClassType*)type;
- (NSUInteger)count;
- (id)head;
- (CNImList*)tail;
- (BOOL)isEmpty;
- (CNImList*)filterF:(BOOL(^)(id))f;
- (CNImList*)reverse;
- (void)forEach:(void(^)(id))each;
- (CNImList*)insertItem:(id)item;
+ (CNEmptyList*)instance;
+ (CNClassType*)type;
@end


@interface CNListIterator : CNIterator_impl {
@protected
    CNImList* _list;
}
@property (nonatomic, retain) CNImList* list;

+ (instancetype)listIterator;
- (instancetype)init;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (CNClassType*)type;
@end


@interface CNImListBuilder : CNBuilder_impl {
@protected
    CNImList* _list;
}
+ (instancetype)imListBuilder;
- (instancetype)init;
- (CNClassType*)type;
- (void)appendItem:(id)item;
- (CNImList*)build;
+ (CNClassType*)type;
@end


