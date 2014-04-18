#import "objdcore.h"
#import "ODObject.h"
@class CNDispatchQueue;
@class CNChain;
@class CNMArray;
@class ODClassType;

@class CNIterableF;
@class CNEmptyIterator;
@protocol CNIterator;
@protocol CNMIterator;
@protocol CNBuilder;
@protocol CNTraversable;
@protocol CNImTraversable;
@protocol CNMTraversable;
@protocol CNIterable;
@protocol CNImIterable;
@protocol CNMIterable;

@protocol CNIterator<NSObject>
- (BOOL)hasNext;
- (id)next;
@end


@protocol CNMIterator<CNIterator>
- (void)remove;
- (void)setValue:(id)value;
@end


@protocol CNBuilder<NSObject>
- (void)appendItem:(id)item;
- (id)build;
- (void)appendAllItems:(id<CNTraversable>)items;
@end


@protocol CNTraversable<NSObject>
- (void)forEach:(void(^)(id))each;
- (void)parForEach:(void(^)(id))each;
- (BOOL)goOn:(BOOL(^)(id))on;
- (CNChain*)chain;
- (id)findWhere:(BOOL(^)(id))where;
- (BOOL)existsWhere:(BOOL(^)(id))where;
- (BOOL)allConfirm:(BOOL(^)(id))confirm;
- (id)head;
- (id)convertWithBuilder:(id<CNBuilder>)builder;
@end


@protocol CNImTraversable<CNTraversable>
- (id<CNMTraversable>)mCopy;
@end


@protocol CNMTraversable<CNTraversable>
- (void)appendItem:(id)item;
- (BOOL)removeItem:(id)item;
- (void)clear;
- (id<CNImTraversable>)im;
- (id<CNImTraversable>)imCopy;
@end


@protocol CNIterable<CNTraversable>
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)head;
- (BOOL)isEmpty;
- (void)forEach:(void(^)(id))each;
- (void)parForEach:(void(^)(id))each;
- (BOOL)goOn:(BOOL(^)(id))on;
- (BOOL)containsItem:(id)item;
- (NSString*)description;
- (NSUInteger)hash;
@end


@protocol CNImIterable<CNIterable, CNImTraversable>
- (id<CNMIterable>)mCopy;
@end


@protocol CNMIterable<CNIterable, CNMTraversable>
- (id<CNMIterator>)mutableIterator;
- (BOOL)removeItem:(id)item;
- (void)mutableFilterBy:(BOOL(^)(id))by;
- (id<CNImIterable>)im;
- (id<CNImIterable>)imCopy;
@end


@interface CNIterableF : NSObject<CNImIterable> {
@protected
    id<CNIterator>(^_iteratorF)();
}
@property (nonatomic, readonly) id<CNIterator>(^iteratorF)();

+ (instancetype)iterableFWithIteratorF:(id<CNIterator>(^)())iteratorF;
- (instancetype)initWithIteratorF:(id<CNIterator>(^)())iteratorF;
- (ODClassType*)type;
- (id<CNIterator>)iterator;
+ (ODClassType*)type;
@end


@interface CNEmptyIterator : NSObject<CNIterator>
+ (instancetype)emptyIterator;
- (instancetype)init;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (CNEmptyIterator*)instance;
+ (ODClassType*)type;
@end


