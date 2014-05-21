#import "objd.h"
#import "CNSet.h"

#import "CNDispatchQueue.h"
#import "CNChain.h"
#import "CNPlat.h"
#import "CNType.h"
@implementation CNSet_impl

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNImSet_impl

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

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNMSet_impl

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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNHashSetBuilder
static CNClassType* _CNHashSetBuilder_type;
@synthesize set = _set;

+ (instancetype)hashSetBuilder {
    return [[CNHashSetBuilder alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _set = [CNMHashSet hashSet];
    
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

- (CNClassType*)type {
    return [CNHashSetBuilder type];
}

+ (CNClassType*)type {
    return _CNHashSetBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end

