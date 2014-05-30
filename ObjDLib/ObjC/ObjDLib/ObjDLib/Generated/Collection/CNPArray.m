#import "objd.h"
#import "CNPArray.h"

#import "CNType.h"
#import "CNString.h"
@implementation CNPArray
static CNClassType* _CNPArray_type;
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
    if(self == [CNPArray class]) _CNPArray_type = [CNClassType classTypeWithCls:[CNPArray class]];
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
    else return ((id)(_wrap(_bytes, index)));
}

- (void)forRefEach:(void(^)(void*))each {
    void* __b = _bytes;
    NSInteger __i = 0;
    while(__i < _count) {
        each(__b);
        __i++;
        __b++;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"PArray(%lu, %lu, %lu, %p, %d)", (unsigned long)_stride, (unsigned long)_count, (unsigned long)_length, _bytes, _copied];
}

- (CNClassType*)type {
    return [CNPArray type];
}

+ (CNClassType*)type {
    return _CNPArray_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNPArrayIterator
static CNClassType* _CNPArrayIterator_type;
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
    if(self == [CNPArrayIterator class]) _CNPArrayIterator_type = [CNClassType classTypeWithCls:[CNPArrayIterator class]];
}

- (BOOL)hasNext {
    return _i < _array.count;
}

- (id)next {
    id ret = [_array applyIndex:((NSUInteger)(_i))];
    _i++;
    return ret;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"PArrayIterator(%@)", _array];
}

- (CNClassType*)type {
    return [CNPArrayIterator type];
}

+ (CNClassType*)type {
    return _CNPArrayIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

