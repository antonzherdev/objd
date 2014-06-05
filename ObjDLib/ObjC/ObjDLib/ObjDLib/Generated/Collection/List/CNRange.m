#import "objd.h"
#import "CNRange.h"

#import "CNType.h"
#import "CNString.h"
@implementation CNRange
static CNClassType* _CNRange_type;
@synthesize start = _start;
@synthesize end = _end;
@synthesize step = _step;
@synthesize count = _count;

+ (instancetype)rangeWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    return [[CNRange alloc] initWithStart:start end:end step:step];
}

- (instancetype)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    self = [super init];
    if(self) {
        _start = start;
        _end = end;
        _step = step;
        _count = ((step > 0) ? ((start <= end) ? ((NSUInteger)((end - start) / step + 1)) : 0) : ((step < 0) ? ((start >= end) ? ((NSUInteger)((end - start) / step + 1)) : 0) : 1));
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNRange class]) _CNRange_type = [CNClassType classTypeWithCls:[CNRange class]];
}

- (id)applyIndex:(NSUInteger)index {
    if(index < _count) return numi(_start + _step * index);
    else return nil;
}

- (id<CNIterator>)iterator {
    return [CNRangeIterator rangeIteratorWithStart:_start end:_end step:_step];
}

- (CNRange*)setStep:(NSInteger)step {
    return [CNRange rangeWithStart:_start end:_end step:step];
}

- (BOOL)isEmpty {
    if(_step > 0) {
        return _start > _end;
    } else {
        if(_step < 0) return _start < _end;
        else return NO;
    }
}

+ (CNRange*)applyI:(NSInteger)i {
    return [CNRange rangeWithStart:i end:i step:1];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"Range(%ld, %ld, %ld)", (long)_start, (long)_end, (long)_step];
}

- (BOOL)isEqual:(id)to {
    if(self == to) return YES;
    if(to == nil || !([to isKindOfClass:[CNRange class]])) return NO;
    CNRange* o = ((CNRange*)(to));
    return _start == o->_start && _end == o->_end && _step == o->_step;
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + _start;
    hash = hash * 31 + _end;
    hash = hash * 31 + _step;
    return hash;
}

- (CNClassType*)type {
    return [CNRange type];
}

+ (CNClassType*)type {
    return _CNRange_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNRangeIterator
static CNClassType* _CNRangeIterator_type;
@synthesize start = _start;
@synthesize end = _end;
@synthesize step = _step;

+ (instancetype)rangeIteratorWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    return [[CNRangeIterator alloc] initWithStart:start end:end step:step];
}

- (instancetype)initWithStart:(NSInteger)start end:(NSInteger)end step:(NSInteger)step {
    self = [super init];
    if(self) {
        _start = start;
        _end = end;
        _step = step;
        _i = start;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNRangeIterator class]) _CNRangeIterator_type = [CNClassType classTypeWithCls:[CNRangeIterator class]];
}

- (BOOL)hasNext {
    return (_step > 0 && _i <= _end) || (_step < 0 && _i >= _end);
}

- (id)next {
    NSInteger ret = _i;
    _i += _step;
    return numi(ret);
}

- (NSString*)description {
    return [NSString stringWithFormat:@"RangeIterator(%ld, %ld, %ld)", (long)_start, (long)_end, (long)_step];
}

- (CNClassType*)type {
    return [CNRangeIterator type];
}

+ (CNClassType*)type {
    return _CNRangeIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

