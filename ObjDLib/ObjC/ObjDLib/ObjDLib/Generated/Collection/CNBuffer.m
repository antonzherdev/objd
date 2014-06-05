#import "objd.h"
#import "CNBuffer.h"

#import "CNType.h"
#import "CNPlatform.h"
#import "CNString.h"
@implementation CNBuffer
static CNClassType* _CNBuffer_type;
@synthesize tp = _tp;
@synthesize count = _count;
@synthesize bytes = _bytes;
@synthesize needFree = _needFree;
@synthesize _pointer = __pointer;
@synthesize _position = __position;

+ (instancetype)bufferWithTp:(CNPType*)tp count:(unsigned int)count bytes:(void*)bytes needFree:(BOOL)needFree {
    return [[CNBuffer alloc] initWithTp:tp count:count bytes:bytes needFree:needFree];
}

- (instancetype)initWithTp:(CNPType*)tp count:(unsigned int)count bytes:(void*)bytes needFree:(BOOL)needFree {
    self = [super init];
    if(self) {
        _tp = tp;
        _count = count;
        _bytes = bytes;
        _needFree = needFree;
        __pointer = bytes;
        __position = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNBuffer class]) _CNBuffer_type = [CNClassType classTypeWithCls:[CNBuffer class]];
}

- (unsigned int)stride {
    return ((unsigned int)(_tp.size));
}

- (NSUInteger)length {
    return ((NSUInteger)([self stride] * _count));
}

- (void)dealloc {
    if(_needFree) cnPointerFree(_bytes);
}

- (void)reset {
    __pointer = _bytes;
    __position = 0;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Buffer(%@, %u, %p, %d)", _tp, _count, _bytes, _needFree];
}

- (CNClassType*)type {
    return [CNBuffer type];
}

+ (CNClassType*)type {
    return _CNBuffer_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNUBuffer
static CNClassType* _CNUBuffer_type;

+ (instancetype)bufferWithTp:(CNPType*)tp count:(unsigned int)count {
    return [[CNUBuffer alloc] initWithTp:tp count:count];
}

- (instancetype)initWithTp:(CNPType*)tp count:(unsigned int)count {
    self = [super initWithTp:tp count:count bytes:cnPointerApplyTpCount(tp, ((NSUInteger)(count))) needFree:YES];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNUBuffer class]) _CNUBuffer_type = [CNClassType classTypeWithCls:[CNUBuffer class]];
}

- (NSString*)description {
    return @"UBuffer";
}

- (CNClassType*)type {
    return [CNUBuffer type];
}

+ (CNClassType*)type {
    return _CNUBuffer_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNInt4Buffer
static CNClassType* _CNInt4Buffer_type;

+ (instancetype)int4BufferWithCount:(unsigned int)count {
    return [[CNInt4Buffer alloc] initWithCount:count];
}

- (instancetype)initWithCount:(unsigned int)count {
    self = [super initWithTp:cnInt4Type() count:count];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNInt4Buffer class]) _CNInt4Buffer_type = [CNClassType classTypeWithCls:[CNInt4Buffer class]];
}

- (int)get {
    if(__position >= _count) @throw @"Out of bound";
    int __il_r = *(((int*)(__pointer)));
    __pointer = ((int*)(__pointer)) + 1;
    __position++;
    return __il_r;
}

- (void)setV:(int)v {
    if(__position >= _count) @throw @"Out of bound";
    *(((int*)(__pointer))) = v;
    __pointer = ((int*)(__pointer)) + 1;
    __position++;
}

- (NSString*)description {
    return @"Int4Buffer";
}

- (CNClassType*)type {
    return [CNInt4Buffer type];
}

+ (CNClassType*)type {
    return _CNInt4Buffer_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNFloat4Buffer
static CNClassType* _CNFloat4Buffer_type;

+ (instancetype)float4BufferWithCount:(unsigned int)count {
    return [[CNFloat4Buffer alloc] initWithCount:count];
}

- (instancetype)initWithCount:(unsigned int)count {
    self = [super initWithTp:cnFloat4Type() count:count];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFloat4Buffer class]) _CNFloat4Buffer_type = [CNClassType classTypeWithCls:[CNFloat4Buffer class]];
}

- (float)get {
    if(__position >= _count) @throw @"Out of bound";
    float __il_r = *(((float*)(__pointer)));
    __pointer = ((float*)(__pointer)) + 1;
    __position++;
    return __il_r;
}

- (void)setV:(float)v {
    if(__position >= _count) @throw @"Out of bound";
    *(((float*)(__pointer))) = v;
    __pointer = ((float*)(__pointer)) + 1;
    __position++;
}

- (NSString*)description {
    return @"Float4Buffer";
}

- (CNClassType*)type {
    return [CNFloat4Buffer type];
}

+ (CNClassType*)type {
    return _CNFloat4Buffer_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

