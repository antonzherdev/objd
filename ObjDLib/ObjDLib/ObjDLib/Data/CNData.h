#import "objdcore.h"
#import "CNSeq.h"
#import "CNCollection.h"
@class ODClassType;
@class CNChain;
@protocol CNSet;
@class CNHashSetBuilder;
@class CNDispatchQueue;

@class CNPArray;
@class CNPArrayIterator;

@interface CNPArray : NSObject<CNImSeq> {
@protected
    NSUInteger _stride;
    id(^_wrap)(void*, NSUInteger);
    NSUInteger _count;
    NSUInteger _length;
    void* _bytes;
    BOOL _copied;
}
@property (nonatomic, readonly) NSUInteger stride;
@property (nonatomic, readonly) id(^wrap)(void*, NSUInteger);
@property (nonatomic, readonly) NSUInteger count;
@property (nonatomic, readonly) NSUInteger length;
@property (nonatomic, readonly) void* bytes;
@property (nonatomic, readonly) BOOL copied;

+ (instancetype)arrayWithStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count length:(NSUInteger)length bytes:(void*)bytes copied:(BOOL)copied;
- (instancetype)initWithStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count length:(NSUInteger)length bytes:(void*)bytes copied:(BOOL)copied;
- (ODClassType*)type;
+ (CNPArray*)applyStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count copyBytes:(void*)copyBytes;
- (id<CNIterator>)iterator;
- (void)dealloc;
- (id)applyIndex:(NSUInteger)index;
+ (ODClassType*)type;
@end


@interface CNPArrayIterator : NSObject<CNIterator> {
@protected
    CNPArray* _array;
    NSInteger _i;
}
@property (nonatomic, readonly) CNPArray* array;

+ (instancetype)arrayIteratorWithArray:(CNPArray*)array;
- (instancetype)initWithArray:(CNPArray*)array;
- (ODClassType*)type;
- (BOOL)hasNext;
- (id)next;
+ (ODClassType*)type;
@end


