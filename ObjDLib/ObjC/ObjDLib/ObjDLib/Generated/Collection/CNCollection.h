#import "objdcore.h"
#import "CNObject.h"
#import "CNEnum.h"
@class CNString;
@class CNClassType;
@class CNImArray;
@class CNDispatchQueue;
@class CNChain;
@class CNMArray;

@class CNIterator_impl;
@class CNMIterator_impl;
@class CNBuilder_impl;
@class CNTraversable_impl;
@class CNImTraversable_impl;
@class CNMTraversable_impl;
@class CNIterable_impl;
@class CNImIterable_impl;
@class CNMIterable_impl;
@class CNIterableF;
@class CNEmptyIterator;
@class CNGo;
@protocol CNIterator;
@protocol CNMIterator;
@protocol CNBuilder;
@protocol CNTraversable;
@protocol CNImTraversable;
@protocol CNMTraversable;
@protocol CNIterable;
@protocol CNImIterable;
@protocol CNMIterable;

typedef enum CNGoR {
    CNGo_Nil = 0,
    CNGo_Continue = 1,
    CNGo_Break = 2
} CNGoR;
@interface CNGo : CNEnum
+ (NSArray*)values;
+ (CNGo*)value:(CNGoR)r;
@end




@protocol CNIterator<NSObject>
- (BOOL)hasNext;
- (id)next;
- (NSString*)description;
@end


@interface CNIterator_impl : NSObject<CNIterator>
+ (instancetype)iterator_impl;
- (instancetype)init;
@end


@protocol CNMIterator<CNIterator>
- (void)remove;
- (void)setValue:(id)value;
- (NSString*)description;
@end


@interface CNMIterator_impl : CNIterator_impl<CNMIterator>
+ (instancetype)iterator_impl;
- (instancetype)init;
@end


@protocol CNBuilder<NSObject>
- (void)appendItem:(id)item;
- (id)build;
- (void)appendAllItems:(id<CNTraversable>)items;
- (NSString*)description;
@end


@interface CNBuilder_impl : NSObject<CNBuilder>
+ (instancetype)builder_impl;
- (instancetype)init;
@end


@protocol CNTraversable<NSObject>
- (void)forEach:(void(^)(id))each;
- (void)parForEach:(void(^)(id))each;
- (CNGoR)goOn:(CNGoR(^)(id))on;
- (CNChain*)chain;
- (BOOL)containsItem:(id)item;
- (id)findWhere:(BOOL(^)(id))where;
- (BOOL)existsWhere:(BOOL(^)(id))where;
- (BOOL)allConfirm:(BOOL(^)(id))confirm;
- (id)head;
- (id)convertWithBuilder:(id<CNBuilder>)builder;
- (NSString*)description;
@end


@interface CNTraversable_impl : NSObject<CNTraversable>
+ (instancetype)traversable_impl;
- (instancetype)init;
@end


@protocol CNImTraversable<CNTraversable>
- (id<CNMTraversable>)mCopy;
- (NSString*)description;
@end


@interface CNImTraversable_impl : CNTraversable_impl<CNImTraversable>
+ (instancetype)imTraversable_impl;
- (instancetype)init;
@end


@protocol CNMTraversable<CNTraversable>
- (void)appendItem:(id)item;
- (BOOL)removeItem:(id)item;
- (void)clear;
- (id<CNImTraversable>)im;
- (id<CNImTraversable>)imCopy;
- (NSString*)description;
@end


@interface CNMTraversable_impl : CNTraversable_impl<CNMTraversable>
+ (instancetype)traversable_impl;
- (instancetype)init;
@end


@protocol CNIterable<CNTraversable>
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)head;
- (BOOL)isEmpty;
- (void)forEach:(void(^)(id))each;
- (void)parForEach:(void(^)(id))each;
- (CNGoR)goOn:(CNGoR(^)(id))on;
- (BOOL)containsItem:(id)item;
- (NSString*)description;
- (NSUInteger)hash;
- (BOOL)isEqualIterable:(id<CNIterable>)iterable;
- (BOOL)isEqual:(id)to;
@end


@interface CNIterable_impl : CNTraversable_impl<CNIterable>
+ (instancetype)iterable_impl;
- (instancetype)init;
- (id)head;
- (BOOL)containsItem:(id)item;
- (NSString*)description;
- (NSUInteger)hash;
@end


@protocol CNImIterable<CNIterable, CNImTraversable>
- (id<CNMIterable>)mCopy;
- (NSString*)description;
@end


@interface CNImIterable_impl : CNIterable_impl<CNImIterable>
+ (instancetype)imIterable_impl;
- (instancetype)init;
- (id<CNMIterable>)mCopy;
@end


@protocol CNMIterable<CNIterable, CNMTraversable>
- (id<CNMIterator>)mutableIterator;
- (BOOL)removeItem:(id)item;
- (void)mutableFilterBy:(BOOL(^)(id))by;
- (id<CNImIterable>)im;
- (id<CNImIterable>)imCopy;
- (NSString*)description;
@end


@interface CNMIterable_impl : CNIterable_impl<CNMIterable>
+ (instancetype)iterable_impl;
- (instancetype)init;
- (BOOL)removeItem:(id)item;
- (id<CNImIterable>)im;
- (id<CNImIterable>)imCopy;
@end


@interface CNIterableF : CNImIterable_impl {
@protected
    id<CNIterator>(^_iteratorF)();
}
@property (nonatomic, readonly) id<CNIterator>(^iteratorF)();

+ (instancetype)iterableFWithIteratorF:(id<CNIterator>(^)())iteratorF;
- (instancetype)initWithIteratorF:(id<CNIterator>(^)())iteratorF;
- (CNClassType*)type;
- (id<CNIterator>)iterator;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNEmptyIterator : CNIterator_impl
+ (instancetype)emptyIterator;
- (instancetype)init;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (NSString*)description;
+ (CNEmptyIterator*)instance;
+ (CNClassType*)type;
@end


