#import "objd.h"
#import "CNData.h"

#import "ODType.h"
@implementation CNPArray
static ODClassType* _CNPArray_type;
@synthesize stride = _stride;
@synthesize wrap = _wrap;
@synthesize count = _count;
@synthesize length = _length;
@synthesize bytes = _bytes;
@synthesize copied = _copied;

+ (instancetype)arrayWithStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count length:(NSUInteger)length bytes:(void*)bytes copied:(BOOL)copied {
    return [[CNPArray alloc] initWithStride:stride wrap:wrap count:count length:length bytes:bytes copied:copied];
}

- (instancetype)initWithStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count length:(NSUInteger)length bytes:(void*)bytes copied:(BOOL)copied {
    self = [super init];
    if(self) {
        _stride = stride;
        _wrap = [wrap copy];
        _count = count;
        _length = length;
        _bytes = bytes;
        _copied = copied;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNPArray class]) _CNPArray_type = [ODClassType classTypeWithCls:[CNPArray class]];
}

+ (CNPArray*)applyStride:(NSUInteger)stride wrap:(id(^)(void*, NSUInteger))wrap count:(NSUInteger)count copyBytes:(void*)copyBytes {
    NSUInteger len = count * stride;
    return [CNPArray arrayWithStride:stride wrap:wrap count:count length:len bytes:cnPointerCopyBytes(copyBytes, count * stride) copied:YES];
}

- (id<CNIterator>)iterator {
    return [CNPArrayIterator arrayIteratorWithArray:self];
}

- (void)dealloc {
    if(_copied) cnPointerFree(_bytes);
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= _count) return nil;
    else return _wrap(_bytes, index);
}

- (ODClassType*)type {
    return [CNPArray type];
}

+ (ODClassType*)type {
    return _CNPArray_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"stride=%lu", (unsigned long)self.stride];
    [description appendFormat:@", count=%lu", (unsigned long)self.count];
    [description appendFormat:@", length=%lu", (unsigned long)self.length];
    [description appendFormat:@", bytes=%p", self.bytes];
    [description appendFormat:@", copied=%d", self.copied];
    [description appendString:@">"];
    return description;
}

@end


@implementation CNPArrayIterator
static ODClassType* _CNPArrayIterator_type;
@synthesize array = _array;

+ (instancetype)arrayIteratorWithArray:(CNPArray*)array {
    return [[CNPArrayIterator alloc] initWithArray:array];
}

- (instancetype)initWithArray:(CNPArray*)array {
    self = [super init];
    if(self) {
        _array = array;
        _i = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNPArrayIterator class]) _CNPArrayIterator_type = [ODClassType classTypeWithCls:[CNPArrayIterator class]];
}

- (BOOL)hasNext {
    return _i < _array.count;
}

- (id)next {
    id ret = ((id)([_array applyIndex:((NSUInteger)(_i))]));
    _i++;
    return ret;
}

- (ODClassType*)type {
    return [CNPArrayIterator type];
}

+ (ODClassType*)type {
    return _CNPArrayIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"array=%@", self.array];
    [description appendString:@">"];
    return description;
}

@end


