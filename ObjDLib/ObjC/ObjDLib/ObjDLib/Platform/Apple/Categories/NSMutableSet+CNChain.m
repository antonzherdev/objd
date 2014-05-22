#import "NSMutableSet+CNChain.h"
#import "NSSet+CNChain.h"
#import "CNEnumerator.h"


@implementation NSMutableSet (CNChain)
+ (CNType *)type {
    static CNClassType* __type = nil;
    if(__type == nil) __type = [CNClassType classTypeWithCls:[NSMutableSet class]];
    return nil;
}

- (CNType*) type {
    return [NSMutableSet type];
}


+ (id)hashSet {
    return [NSMutableSet set];
}

- (void)appendItem:(id)object {
    [self addObject:wrapNil(object)];
}

+ (NSMutableSet *)applyCapacity:(NSUInteger)capacity {
    return [NSMutableSet setWithCapacity:capacity];
}

- (BOOL)removeItem:(id)object {
    NSUInteger oldCount = self.count;
    [self removeObject:wrapNil(object)];
    return oldCount > self.count;
}


- (void)mutableFilterBy:(BOOL(^)(id))by {
    @throw @"Hasn't implemented yet";
}


- (void)clear {
    [self removeAllObjects];
}

- (id <CNMIterator>)mutableIterator {
    return [CNMEnumerator enumeratorWithEnumerator:[self objectEnumerator]];
}

- (NSSet*)im {
    return self;
}

- (id <CNImSet>)imCopy {
    return [NSSet setWithSet:self];
}

@end