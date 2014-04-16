#import "objdcore.h"
#import "CNSeq.h"
#import "CNCollection.h"
@class ODClassType;
@class CNChain;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNDispatchQueue;

@class CNImList;
@class CNFilledList;
@class CNEmptyList;
@class CNListIterator;
@class CNImListBuilder;

@interface CNImList : NSObject<CNImSeq>
+ (instancetype)imList;
- (instancetype)init;
- (ODClassType*)type;
+ (CNImList*)apply;
+ (CNImList*)applyItem:(id)item;
+ (CNImList*)applyItem:(id)item tail:(CNImList*)tail;
- (id<CNIterator>)iterator;
- (CNImList*)tail;
- (CNImList*)filterF:(BOOL(^)(id))f;
- (CNImList*)reverse;
- (CNImList*)insertItem:(id)item;
+ (ODClassType*)type;
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
- (ODClassType*)type;
- (id)head;
- (BOOL)isEmpty;
- (CNImList*)filterF:(BOOL(^)(id))f;
- (CNImList*)reverse;
- (void)forEach:(void(^)(id))each;
- (CNImList*)insertItem:(id)item;
+ (ODClassType*)type;
@end


@interface CNEmptyList : CNImList
+ (instancetype)emptyList;
- (instancetype)init;
- (ODClassType*)type;
- (NSUInteger)count;
- (id)head;
- (CNImList*)tail;
- (BOOL)isEmpty;
- (CNImList*)filterF:(BOOL(^)(id))f;
- (CNImList*)reverse;
- (void)forEach:(void(^)(id))each;
- (CNImList*)insertItem:(id)item;
+ (CNEmptyList*)instance;
+ (ODClassType*)type;
@end


@interface CNListIterator : NSObject<CNIterator> {
@protected
    CNImList* _list;
}
@property (nonatomic, retain) CNImList* list;

+ (instancetype)listIterator;
- (instancetype)init;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


@interface CNImListBuilder : NSObject<CNBuilder> {
@protected
    CNImList* _list;
}
+ (instancetype)imListBuilder;
- (instancetype)init;
- (ODClassType*)type;
- (void)appendItem:(id)item;
- (CNImList*)build;
+ (ODClassType*)type;
@end


