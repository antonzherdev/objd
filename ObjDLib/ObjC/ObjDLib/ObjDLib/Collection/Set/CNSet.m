#import "objd.h"
#import "CNSet.h"

#import "CNType.h"
#import "CNDispatchQueue.h"
#import "CNObject.h"
#import "CNChain.h"
#import "CNPlat.h"
@implementation CNSet_impl

+ (instancetype)set_impl {
    return [[CNSet_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNImSet_impl

+ (instancetype)imSet_impl {
    return [[CNImSet_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (id<CNMSet>)mCopy {
    CNMHashSet* arr = [CNMHashSet hashSet];
    {
        id<CNIterator> __il__1i = [self iterator];
        while([__il__1i hasNext]) {
            id item = [__il__1i next];
            [arr appendItem:item];
        }
    }
    return arr;
}

- (id<CNIterator>)iterator {
    @throw @"Method iterator is abstract";
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

@implementation CNMSet_impl

+ (instancetype)set_impl {
    return [[CNMSet_impl alloc] init];
}

- (instancetype)init {
    self = [super init];
    
    return self;
}

- (id<CNImSet>)im {
    return [self imCopy];
}

- (id<CNImSet>)imCopy {
    CNMHashSet* arr = [CNMHashSet hashSet];
    {
        id<CNIterator> __il__1i = [self iterator];
        while([__il__1i hasNext]) {
            id item = [__il__1i next];
            [arr appendItem:item];
        }
    }
    return [arr im];
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

@implementation CNHashSetBuilder
static CNClassType* _CNHashSetBuilder_type;
@synthesize set = _set;

+ (instancetype)hashSetBuilderWithCapacity:(NSUInteger)capacity {
    return [[CNHashSetBuilder alloc] initWithCapacity:capacity];
}

- (instancetype)initWithCapacity:(NSUInteger)capacity {
    self = [super init];
    if(self) _set = [CNMHashSet applyCapacity:capacity];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNHashSetBuilder class]) _CNHashSetBuilder_type = [CNClassType classTypeWithCls:[CNHashSetBuilder class]];
}

- (void)appendItem:(id)item {
    [_set appendItem:item];
}

- (CNImHashSet*)build {
    return [_set im];
}

+ (CNHashSetBuilder*)apply {
    return [CNHashSetBuilder hashSetBuilderWithCapacity:0];
}

- (NSString*)description {
    return @"HashSetBuilder";
}

- (CNClassType*)type {
    return [CNHashSetBuilder type];
}

+ (CNClassType*)type {
    return _CNHashSetBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end

