#import "objdcore.h"
#import "CNSeq.h"
#import "ODObject.h"
#import "CNCollection.h"
@class ODClassType;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNChain;

@class CNMutableList;
@class CNMutableListItem;
@class CNMutableListIterator;
@class CNMutableListImmutableIterator;

@interface CNMutableList : NSObject<CNMutableSeq>
+ (id)mutableList;
- (id)init;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNMutableIterator>)mutableIterator;
- (void)appendItem:(id)item;
- (void)removeListItem:(CNMutableListItem*)listItem;
- (void)clear;
- (void)forEach:(void(^)(id))each;
- (BOOL)goOn:(BOOL(^)(id))on;
- (void)mutableFilterBy:(BOOL(^)(id))by;
+ (ODClassType*)type;
@end


@interface CNMutableListItem : NSObject
@property (nonatomic, retain) id data;
@property (nonatomic, retain) CNMutableListItem* next;
@property (nonatomic, weak) CNMutableListItem* prev;

+ (id)mutableListItem;
- (id)init;
- (ODClassType*)type;
+ (ODClassType*)type;
@end


@interface CNMutableListIterator : NSObject<CNMutableIterator>
@property (nonatomic, readonly) CNMutableList* list;
@property (nonatomic, retain) CNMutableListItem* item;

+ (id)mutableListIteratorWithList:(CNMutableList*)list;
- (id)initWithList:(CNMutableList*)list;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (void)remove;
+ (ODClassType*)type;
@end


@interface CNMutableListImmutableIterator : NSObject<CNIterator>
@property (nonatomic, weak) CNMutableListItem* item;

+ (id)mutableListImmutableIterator;
- (id)init;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


