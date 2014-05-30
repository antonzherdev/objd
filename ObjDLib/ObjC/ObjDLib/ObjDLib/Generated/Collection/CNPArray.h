#import "objdcore.h"
#import "CNSeq.h"
#import "CNObject.h"
#import "CNPointer.h"
#import "CNCollection.h"
@class CNClassType;
@class CNString;

@class CNPArray;
@class CNPArrayIterator;

@interface CNPArray : CNImSeq_impl {
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
- (CNClassType*)type;
+ (CNPArray*)applyStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count copyBytes:(void*)copyBytes;
- (id<CNIterator>)iterator;
- (void)dealloc;
- (id)applyIndex:(NSUInteger)index;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNPArrayIterator : CNIterator_impl {
@protected
    CNPArray* _array;
    NSInteger _i;
}
@property (nonatomic, readonly) CNPArray* array;

+ (instancetype)arrayIteratorWithArray:(CNPArray*)array;
- (instancetype)initWithArray:(CNPArray*)array;
- (CNClassType*)type;
- (BOOL)hasNext;
- (id)next;
- (NSString*)description;
+ (CNClassType*)type;
@end


