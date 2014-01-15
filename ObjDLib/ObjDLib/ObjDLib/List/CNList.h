#import "objdcore.h"
#import "CNSeq.h"
#import "ODObject.h"
#import "CNCollection.h"
@class ODClassType;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNChain;

@class CNList;
@class CNFilledList;
@class CNEmptyList;
@class CNListIterator;

@interface CNList : NSObject<CNSeq>
+ (id)list;
- (id)init;
- (ODClassType*)type;
+ (CNList*)apply;
+ (CNList*)applyItem:(id)item;
+ (CNList*)applyItem:(id)item tail:(CNList*)tail;
- (id<CNIterator>)iterator;
- (CNList*)tail;
- (CNList*)filterF:(BOOL(^)(id))f;
- (CNList*)reverse;
+ (ODClassType*)type;
@end


@interface CNFilledList : CNList
@property (nonatomic, readonly) id head;
@property (nonatomic, readonly) CNList* tail;
@property (nonatomic, readonly) NSUInteger count;

+ (id)filledListWithHead:(id)head tail:(CNList*)tail;
- (id)initWithHead:(id)head tail:(CNList*)tail;
- (ODClassType*)type;
- (id)headOpt;
- (BOOL)isEmpty;
- (CNList*)filterF:(BOOL(^)(id))f;
- (CNList*)reverse;
- (void)forEach:(void(^)(id))each;
+ (ODClassType*)type;
@end


@interface CNEmptyList : CNList
+ (id)emptyList;
- (id)init;
- (ODClassType*)type;
- (NSUInteger)count;
- (id)head;
- (id)headOpt;
- (CNList*)tail;
- (BOOL)isEmpty;
- (CNList*)filterF:(BOOL(^)(id))f;
- (CNList*)reverse;
- (void)forEach:(void(^)(id))each;
+ (CNEmptyList*)instance;
+ (ODClassType*)type;
@end


@interface CNListIterator : NSObject<CNIterator>
@property (nonatomic, retain) CNList* list;

+ (id)listIterator;
- (id)init;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


