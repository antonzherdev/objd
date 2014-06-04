#import "objd.h"
#import "CNBuffer.h"

#import "CNType.h"
#import "CNPlatform.h"
#import "CNString.h"
@implementation CNBuffer
static CNClassType* _CNBuffer_type;

+ (void)initialize {
    [super initialize];
    if(self == [CNBuffer class]) _CNBuffer_type = [CNClassType classTypeWithCls:[CNBuffer class]];
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

@implementation CNInt4Buffer
static CNClassType* _CNInt4Buffer_type;
@synthesize tp = _tp;
@synthesize count = _count;
@synthesize bytes = _bytes;

+ (instancetype)int4BufferWithCount:(unsigned int)count {
    return [[CNInt4Buffer alloc] initWithCount:count];
}

- (instancetype)initWithCount:(unsigned int)count {
    self = [super init];
    if(self) {
        _count = count;
        _tp = cnInt4Type();
        _count = count;
        _bytes = cnPointerApplyTpCount(cnInt4Type(), ((NSUInteger)(count)));
        __pointer = _bytes;
        __position = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNInt4Buffer class]) _CNInt4Buffer_type = [CNClassType classTypeWithCls:[CNInt4Buffer class]];
}

- (NSString*)description {
    return @"Int4Buffer";
}

- (unsigned int)stride {
    return ((unsigned int)(_tp.size));
}

- (NSUInteger)length {
    return ((NSUInteger)([self stride] * _count));
}

- (void)dealloc {
    cnPointerFree(_bytes);
}

- (int)get {
    if(__position >= _count) @throw @"Out of bound";
    id r = numi4(*(__pointer));
    __pointer++;
    __position++;
    return unumi4(r);
}

- (void)setV:(int)v {
    if(__position >= _count) @throw @"Out of bound";
    *(__pointer) = v;
    __pointer++;
    __position++;
}

- (void)reset {
    __pointer = _bytes;
    __position = 0;
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
@synthesize tp = _tp;
@synthesize count = _count;
@synthesize bytes = _bytes;

+ (instancetype)float4BufferWithCount:(unsigned int)count {
    return [[CNFloat4Buffer alloc] initWithCount:count];
}

- (instancetype)initWithCount:(unsigned int)count {
    self = [super init];
    if(self) {
        _count = count;
        _tp = cnFloat4Type();
        _count = count;
        _bytes = cnPointerApplyTpCount(cnFloat4Type(), ((NSUInteger)(count)));
        __pointer = _bytes;
        __position = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFloat4Buffer class]) _CNFloat4Buffer_type = [CNClassType classTypeWithCls:[CNFloat4Buffer class]];
}

- (NSString*)description {
    return @"Float4Buffer";
}

- (unsigned int)stride {
    return ((unsigned int)(_tp.size));
}

- (NSUInteger)length {
    return ((NSUInteger)([self stride] * _count));
}

- (void)dealloc {
    cnPointerFree(_bytes);
}

- (float)get {
    if(__position >= _count) @throw @"Out of bound";
    id r = numf4(*(__pointer));
    __pointer++;
    __position++;
    return unumf4(r);
}

- (void)setV:(float)v {
    if(__position >= _count) @throw @"Out of bound";
    *(__pointer) = v;
    __pointer++;
    __position++;
}

- (void)reset {
    __pointer = _bytes;
    __position = 0;
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

