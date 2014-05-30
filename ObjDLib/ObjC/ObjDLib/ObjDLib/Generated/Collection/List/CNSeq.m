#import "objd.h"
#import "CNSeq.h"

#import "CNSet.h"
#import "CNType.h"
#import "CNObject.h"
#import "CNDispatchQueue.h"
#import "CNChain.h"
#import "CNPlat.h"
@implementation CNSeq_impl

+ (instancetype)seq_impl {
    return [[CNSeq_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (BOOL)isEmpty {
    return [self count] == 0;
}

- (id)head {
    return [self applyIndex:0];
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= [self count]) return nil;
    id<CNIterator> i = [self iterator];
    NSUInteger n = index;
    while([i hasNext]) {
        if(n == 0) return [i next];
        [i next];
        n--;
    }
    return nil;
}

- (id<CNSet>)toSet {
    return [self convertWithBuilder:[CNHashSetBuilder apply]];
}

- (BOOL)isEqualSeq:(id<CNSeq>)seq {
    if([self count] != [seq count]) return NO;
    id<CNIterator> ia = [self iterator];
    id<CNIterator> ib = [seq iterator];
    while([ia hasNext] && [ib hasNext]) {
        if(!([[ia next] isEqual:[ib next]])) return NO;
    }
    return YES;
}

- (id)last {
    return [self applyIndex:[self count] - 1];
}

- (id<CNImSeq>)tail {
    CNArrayBuilder* builder = [CNArrayBuilder apply];
    id<CNIterator> i = [self iterator];
    if([i hasNext]) {
        [i next];
        while([i hasNext]) {
            [builder appendItem:[i next]];
        }
    }
    return [builder build];
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNImSeq_impl

+ (instancetype)imSeq_impl {
    return [[CNImSeq_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (id<CNMSeq>)mCopy {
    CNMArray* arr = [CNMArray array];
    {
        id<CNIterator> __il__1i = [self iterator];
        while([__il__1i hasNext]) {
            id item = [__il__1i next];
            [arr appendItem:item];
        }
    }
    return arr;
}

- (id<CNImSeq>)addItem:(id)item {
    CNArrayBuilder* builder = [CNArrayBuilder apply];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
}

- (id<CNImSeq>)addSeq:(id<CNSeq>)seq {
    CNArrayBuilder* builder = [CNArrayBuilder apply];
    [builder appendAllItems:self];
    [builder appendAllItems:seq];
    return [builder build];
}

- (id<CNImSeq>)subItem:(id)item {
    return [[[self chain] filterWhen:^BOOL(id _) {
        return !([_ isEqual:item]);
    }] toArray];
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMSeq_impl

+ (instancetype)seq_impl {
    return [[CNMSeq_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (id<CNImSeq>)im {
    return [self imCopy];
}

- (id<CNImSeq>)imCopy {
    CNMArray* arr = [CNMArray array];
    {
        id<CNIterator> __il__1i = [self iterator];
        while([__il__1i hasNext]) {
            id item = [__il__1i next];
            [arr appendItem:item];
        }
    }
    return arr;
}

- (BOOL)removeIndex:(NSUInteger)index {
    id<CNMIterator> i = [self mutableIterator];
    NSUInteger j = index;
    BOOL ret = NO;
    while([i hasNext]) {
        [i next];
        if(j == 0) {
            [i remove];
            ret = YES;
            break;
        }
        j--;
    }
    return ret;
}

- (void)insertIndex:(NSUInteger)index item:(id)item {
    @throw @"Method insert is abstract";
}

- (void)prependItem:(id)item {
    @throw @"Method prepend is abstract";
}

- (void)setIndex:(NSUInteger)index item:(id)item {
    id<CNMIterator> i = [self mutableIterator];
    NSUInteger n = index;
    while([i hasNext]) {
        if(n == 0) {
            [i next];
            [i setValue:item];
            return ;
        }
        [i next];
        n--;
    }
    @throw @"Incorrect index";
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id<CNMIterator>)mutableIterator {
    @throw @"Method mutableIterator is abstract";
}

- (BOOL)removeItem:(id)item {
    id<CNMIterator> i = [self mutableIterator];
    BOOL ret = NO;
    while([i hasNext]) {
        if([[i next] isEqual:item]) {
            [i remove];
            ret = YES;
        }
    }
    return ret;
}

- (void)mutableFilterBy:(BOOL(^)(id))by {
    id<CNMIterator> i = [self mutableIterator];
    while([i hasNext]) {
        if(by([i next])) [i remove];
    }
}

- (void)appendItem:(id)item {
    @throw @"Method append is abstract";
}

- (void)clear {
    @throw @"Method clear is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNArrayBuilder
static CNClassType* _CNArrayBuilder_type;

+ (instancetype)arrayBuilderWithCapacity:(NSUInteger)capacity {
    return [[CNArrayBuilder alloc] initWithCapacity:capacity];
}

- (instancetype)initWithCapacity:(NSUInteger)capacity {
    self = [super init];
    if(self) _array = [CNMArray applyCapacity:capacity];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNArrayBuilder class]) _CNArrayBuilder_type = [CNClassType classTypeWithCls:[CNArrayBuilder class]];
}

- (void)appendItem:(id)item {
    [_array appendItem:item];
}

- (CNImArray*)build {
    return _array;
}

+ (CNArrayBuilder*)apply {
    return [CNArrayBuilder arrayBuilderWithCapacity:0];
}

- (NSString*)description {
    return @"ArrayBuilder";
}

- (CNClassType*)type {
    return [CNArrayBuilder type];
}

+ (CNClassType*)type {
    return _CNArrayBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNIndexFunSeq
static CNClassType* _CNIndexFunSeq_type;
@synthesize count = _count;
@synthesize f = _f;

+ (instancetype)indexFunSeqWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    return [[CNIndexFunSeq alloc] initWithCount:count f:f];
}

- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    self = [super init];
    if(self) {
        _count = count;
        _f = [f copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNIndexFunSeq class]) _CNIndexFunSeq_type = [CNClassType classTypeWithCls:[CNIndexFunSeq class]];
}

- (id)applyIndex:(NSUInteger)index {
    if(index >= _count) return nil;
    else return ((id)(_f(index)));
}

- (id<CNIterator>)iterator {
    return [CNIndexFunSeqIterator indexFunSeqIteratorWithCount:_count f:_f];
}

- (NSString*)description {
    return [NSString stringWithFormat:@"IndexFunSeq(%lu)", (unsigned long)_count];
}

- (CNClassType*)type {
    return [CNIndexFunSeq type];
}

+ (CNClassType*)type {
    return _CNIndexFunSeq_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNIndexFunSeqIterator
static CNClassType* _CNIndexFunSeqIterator_type;
@synthesize count = _count;
@synthesize f = _f;

+ (instancetype)indexFunSeqIteratorWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    return [[CNIndexFunSeqIterator alloc] initWithCount:count f:f];
}

- (instancetype)initWithCount:(NSUInteger)count f:(id(^)(NSUInteger))f {
    self = [super init];
    if(self) {
        _count = count;
        _f = [f copy];
        _i = 0;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNIndexFunSeqIterator class]) _CNIndexFunSeqIterator_type = [CNClassType classTypeWithCls:[CNIndexFunSeqIterator class]];
}

- (BOOL)hasNext {
    return _i < _count;
}

- (id)next {
    id ret = _f(_i);
    _i++;
    return ret;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"IndexFunSeqIterator(%lu)", (unsigned long)_count];
}

- (CNClassType*)type {
    return [CNIndexFunSeqIterator type];
}

+ (CNClassType*)type {
    return _CNIndexFunSeqIterator_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

