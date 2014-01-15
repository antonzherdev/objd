#import "objdcore.h"
#import "CNSeq.h"
#import "ODObject.h"
#import "CNCollection.h"
@class ODClassType;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNChain;

@class CNOption;
@class CNNone;
@class CNSome;
@class CNSomeIterator;

@interface CNOption : NSObject<CNSeq>
+ (id)option;
- (id)init;
- (ODClassType*)type;
+ (id)none;
+ (id)applyValue:(id)value;
+ (id)someValue:(id)value;
- (id)get;
- (id)getOrElseF:(id(^)())f;
- (id)getOrValue:(id)value;
- (id)getOrNil;
- (id)mapF:(id(^)(id))f;
- (id)flatMapF:(id(^)(id))f;
- (id)filterF:(BOOL(^)(id))f;
- (BOOL)isEmpty;
- (BOOL)isDefined;
- (void)forEach:(void(^)(id))each;
- (BOOL)tryEach:(void(^)(id))each;
- (id<CNIterator>)iterator;
+ (ODClassType*)type;
@end


@interface CNNone : CNOption
+ (id)none;
- (id)init;
- (ODClassType*)type;
- (NSUInteger)count;
- (id)get;
- (id)getOrElseF:(id(^)())f;
- (id)getOrValue:(id)value;
- (id)getOrNil;
- (void)forEach:(void(^)(id))each;
- (id)mapF:(id(^)(id))f;
- (id)flatMapF:(id(^)(id))f;
- (id)filterF:(BOOL(^)(id))f;
- (BOOL)isEmpty;
- (BOOL)isDefined;
- (id<CNIterator>)iterator;
- (BOOL)goOn:(BOOL(^)(id))on;
- (BOOL)tryEach:(void(^)(id))each;
- (BOOL)containsItem:(id)item;
+ (ODClassType*)type;
@end


@interface CNSome : CNOption
@property (nonatomic, readonly) id value;

+ (id)someWithValue:(id)value;
- (id)initWithValue:(id)value;
- (ODClassType*)type;
- (NSUInteger)count;
- (id)get;
- (id)getOrElseF:(id(^)())f;
- (id)getOrNil;
- (id)getOrValue:(id)value;
- (id)mapF:(id(^)(id))f;
- (id)flatMapF:(id(^)(id))f;
- (id)filterF:(BOOL(^)(id))f;
- (BOOL)isEmpty;
- (BOOL)isDefined;
- (id<CNIterator>)iterator;
- (void)forEach:(void(^)(id))each;
- (BOOL)tryEach:(void(^)(id))each;
- (BOOL)goOn:(BOOL(^)(id))on;
- (BOOL)containsItem:(id)item;
+ (ODClassType*)type;
@end


@interface CNSomeIterator : NSObject<CNIterator>
@property (nonatomic, readonly) id value;
@property (nonatomic) BOOL hasNext;

+ (id)someIteratorWithValue:(id)value;
- (id)initWithValue:(id)value;
- (ODClassType*)type;
- (id)next;
+ (ODClassType*)type;
@end


